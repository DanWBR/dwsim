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
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker
Imports System.ComponentModel
Imports AeroWizard

Public Class FormSimulWizard

    Public CurrentFlowsheet As FormFlowsheet
    Dim loaded As Boolean = False

    Private prevsort As System.ComponentModel.ListSortDirection = System.ComponentModel.ListSortDirection.Ascending
    Private prevcol As Integer = 1

    Public switch As Boolean = False

    Private CompoundList As List(Of String)
    Private Indexes As Dictionary(Of String, Integer)

    Public Shared AddMorePages As Action(Of StepWizardControl, IFlowsheet)

    Public Shared WizardFinished As Action(Of StepWizardControl, IFlowsheet)
    Public Shared WizardFinished2 As Action(Of StepWizardControl, IFlowsheet)
    Public Shared WizardFinished3 As Action(Of StepWizardControl, IFlowsheet)

    Private Sub FormConfigWizard_Load(sender As Object, e As System.EventArgs) Handles Me.Load

        AddMorePages?.Invoke(StepWizardControl1, CurrentFlowsheet)

        ExtensionMethods.ChangeDefaultFont(Me)

        Me.StepWizardControl1.FinishButtonText = DWSIM.App.GetLocalString("FinishText")
        Me.StepWizardControl1.CancelButtonText = DWSIM.App.GetLocalString("CancelText")
        Me.StepWizardControl1.NextButtonText = DWSIM.App.GetLocalString("NextText") & " >"

        ogc1.RowTemplate.Height = 23 * Settings.DpiScale
        ogc1.ColumnHeadersHeight *= Settings.DpiScale

        DataGridViewPP.Columns(2).Width = 24 * Settings.DpiScale
        DataGridViewPP.Columns(3).Width = 24 * Settings.DpiScale

        Init()

        FormMain.TranslateFormFunction?.Invoke(Me)

    End Sub

    Sub Init(Optional ByVal reset As Boolean = False)

        Dim pathsep As Char = Path.DirectorySeparatorChar

        Dim comp As BaseClasses.ConstantProperties
        If Not loaded Or reset Then

            colAdd.ValueType = True.GetType()
            colAdd.FalseValue = False
            colAdd.TrueValue = True
            colAdd.IndeterminateValue = False

            txtSearch.AutoCompleteCustomSource = New AutoCompleteStringCollection()
            CompoundList = New List(Of String)()
            Indexes = New Dictionary(Of String, Integer)
            Dim rowlist As New List(Of DataGridViewRow)
            ogc1.Rows.Clear()
            For Each comp In Me.CurrentFlowsheet.Options.SelectedComponents.Values
                Dim r As New DataGridViewRow()
                Dim data = New Object() {comp.Name, True, comp.Name, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.CurrentDB, comp.IsCOOLPROPSupported}
                r.CreateCells(ogc1, data)
                r.Height = 23 * Settings.DpiScale
                rowlist.Add(r)
                CompoundList.Add(comp.Name)
                CompoundList.Add(comp.CAS_Number)
                CompoundList.Add(comp.Formula)
                Indexes.Add(comp.Name, ogc1.Rows.Count - 1)
                Indexes.Add(comp.CAS_Number, ogc1.Rows.Count - 1)
                If Not Indexes.ContainsKey(comp.Formula) Then Indexes.Add(comp.Formula, ogc1.Rows.Count - 1)
            Next
            For Each comp In Me.CurrentFlowsheet.Options.NotSelectedComponents.Values.OrderBy(Function(c) c.ChemSepFamily)
                Dim r As New DataGridViewRow()
                Dim data = New Object() {comp.Name, False, comp.Name, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.CurrentDB, comp.IsCOOLPROPSupported}
                r.CreateCells(ogc1, data)
                r.Height = 23 * Settings.DpiScale
                rowlist.Add(r)
                CompoundList.Add(comp.CAS_Number)
                CompoundList.Add(comp.Formula)
                If Not Indexes.ContainsKey(comp.Name) Then Indexes.Add(comp.Name, ogc1.Rows.Count - 1)
                If Not Indexes.ContainsKey(comp.CAS_Number) Then Indexes.Add(comp.CAS_Number, ogc1.Rows.Count - 1)
                If Not Indexes.ContainsKey(comp.Formula) Then Indexes.Add(comp.Formula, ogc1.Rows.Count - 1)
            Next
            ogc1.Rows.AddRange(rowlist.ToArray())
            txtSearch.AutoCompleteCustomSource.AddRange(CompoundList.ToArray())

            'property packages
            Me.DataGridViewPP.Rows.Clear()
            For Each pp2 As PropertyPackages.PropertyPackage In FormMain.PropertyPackages.Values.OrderBy(Function(x) x.ComponentName)
                Me.DataGridViewPP.Rows.Add(New Object() {pp2.ComponentName, 0, Nothing, pp2.GetDisplayIcon(), pp2.ComponentName, pp2.ComponentDescription})
            Next

            If Not FormMain.IsPro Then
                ProFeatures.Functions.AddProPPs(DataGridViewPP)
            End If

            DataGridViewPP.Sort(DataGridViewPP.Columns(3), System.ComponentModel.ListSortDirection.Ascending)

        Else

            For Each r As DataGridViewRow In ogc1.Rows
                If CurrentFlowsheet.Options.NotSelectedComponents.ContainsKey(r.Cells(0).Value) Then
                    comp = CurrentFlowsheet.Options.NotSelectedComponents(r.Cells(0).Value)
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
            For Each pp2 As PropertyPackages.PropertyPackage In CurrentFlowsheet.Options.PropertyPackages.Values
                .Add(New Object() {pp2.UniqueID, pp2.Tag, pp2.ComponentName})
            Next
        End With

        Dim array1(FormMain.AvailableUnitSystems.Count - 1) As String
        FormMain.AvailableUnitSystems.Keys.CopyTo(array1, 0)
        Me.ComboBox2.Items.Clear()
        Me.ComboBox2.Items.AddRange(array1)

        ComboBox2.SelectedIndex = 0

        cbPPFilter.SelectedIndex = 0

        chkDoubleClickToOpenEditors.Checked = My.Settings.DoubleClickToEdit

        Me.loaded = True

    End Sub

    Private Sub LinkLabelPropertyMethods_LinkClicked(sender As System.Object, e As System.Windows.Forms.LinkLabelLinkClickedEventArgs)
        Process.Start("https://dwsim.org/wiki/index.php?title=Property_Methods_and_Correlation_Profiles")
    End Sub

    Private Sub TextBox1_KeyDown(sender As Object, e As System.Windows.Forms.KeyEventArgs) Handles txtSearch.KeyDown
        If e.KeyCode = Keys.Enter Then
            If Me.ogc1.SelectedRows.Count > 0 Then
                Me.ogc1.SelectedRows(0).Cells(1).Value = Not Me.ogc1.SelectedRows(0).Cells(1).Value
            End If
        End If
    End Sub

    Private Sub TextBox1_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtSearch.TextChanged

        ogc1.SuspendLayout()

        Try

            ogc1.ClearSelection()

            If txtSearch.Text = "" Then
                For Each r As DataGridViewRow In ogc1.Rows
                    r.Selected = False
                    r.Visible = True
                Next
                ogc1.FirstDisplayedScrollingRowIndex = 0
                ogc1.Sort(colAdd, System.ComponentModel.ListSortDirection.Descending)
            Else
                For Each r As DataGridViewRow In ogc1.Rows
                    If Not r.Cells(2).Value Is Nothing Then
                        If r.Cells(2).Value.ToString.ToLower.Contains(txtSearch.Text.ToLower) Or
                       r.Cells(3).Value.ToString.ToLower.Contains(txtSearch.Text.ToLower) Or
                       r.Cells(5).Value.ToString.ToLower.Contains(txtSearch.Text.ToLower) Then
                            r.Visible = True
                            If r.Cells(2).Value.ToString.ToLower.Equals(txtSearch.Text.ToLower) Or
                                           r.Cells(3).Value.ToString.ToLower.Equals(txtSearch.Text.ToLower) Or
                                           r.Cells(5).Value.ToString.ToLower.Equals(txtSearch.Text.ToLower) Then
                                r.Selected = True
                            End If
                        Else
                            r.Visible = False
                        End If
                    End If
                Next
                'ogc1.Sort(colName, System.ComponentModel.ListSortDirection.Ascending)
                If ogc1.SelectedRows.Count > 0 Then
                    ogc1.FirstDisplayedScrollingRowIndex = ogc1.SelectedRows(0).Index
                End If
            End If

        Catch ex As Exception

        End Try

        ogc1.ResumeLayout()

    End Sub

    Private Sub Button7_Click(sender As System.Object, e As System.EventArgs)
        If Me.ogc1.SelectedRows.Count > 0 Then
            Me.AddCompToSimulation(Me.ogc1.SelectedRows(0).Index)
        End If
    End Sub

    Sub AddCompToSimulation(ByVal compid As String)
        ' TODO Add code to check that index is within range. If it is out of range, don't do anything.
        If Me.loaded Then
            If Not Me.CurrentFlowsheet.Options.SelectedComponents.ContainsKey(compid) Then
                Dim tmpcomp As New BaseClasses.ConstantProperties
                tmpcomp = Me.CurrentFlowsheet.Options.NotSelectedComponents(compid)

                If tmpcomp.OriginalDB = "ChemSep" Then
                    FormMain.AnalyticsProvider?.RegisterEvent("Compound Added", tmpcomp.Name, Nothing)
                End If

                Me.CurrentFlowsheet.Options.SelectedComponents.Add(tmpcomp.Name, tmpcomp)
                Me.CurrentFlowsheet.Options.NotSelectedComponents.Remove(tmpcomp.Name)
                Dim ms As Streams.MaterialStream

                Dim proplist As New ArrayList
                For Each ms In CurrentFlowsheet.Collections.FlowsheetObjectCollection.Values
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
        tmpcomp = Me.CurrentFlowsheet.Options.SelectedComponents(nm)

        If tmpcomp.OriginalDB = "ChemSep" Then
            FormMain.AnalyticsProvider?.RegisterEvent("Compound Removed", tmpcomp.Name, Nothing)
        End If

        Me.CurrentFlowsheet.Options.SelectedComponents.Remove(tmpcomp.Name)
        Me.CurrentFlowsheet.Options.NotSelectedComponents.Add(tmpcomp.Name, tmpcomp)
        Dim ms As Streams.MaterialStream
        Dim proplist As New ArrayList

        For Each ms In CurrentFlowsheet.Collections.FlowsheetObjectCollection.Values
            For Each phase As BaseClasses.Phase In ms.Phases.Values
                phase.Compounds.Remove(tmpcomp.Name)
            Next
        Next
    End Sub

    Private Sub Button8_Click(sender As System.Object, e As System.EventArgs) Handles Button8.Click

        If Integer.TryParse(DataGridViewPP.SelectedRows(0).Cells(0).Value, New Integer) Then

            ProFeatures.Functions.CreateTransitionObject(CurrentFlowsheet, DataGridViewPP.SelectedRows(0).Cells(4).Value, "Property Package", "Add", "Simulation Wizard", Nothing)

            ProFeatures.Functions.DisplayTransitionForm(CurrentFlowsheet, DataGridViewPP.SelectedRows(0).Cells(4).Value + " Property Package")

            Exit Sub

        End If
        Dim pp As PropertyPackages.PropertyPackage
        pp = FormMain.PropertyPackages(Me.DataGridViewPP.SelectedRows(0).Cells(0).Value).Clone
        With pp
            pp.Tag = pp.ComponentName + " (" + (CurrentFlowsheet.PropertyPackages.Count + 1).ToString() + ")"
            pp.UniqueID = "PP-" & Guid.NewGuid.ToString
            pp.Flowsheet = CurrentFlowsheet
        End With

        FormMain.AnalyticsProvider?.RegisterEvent("Property Package Added", pp.ComponentName, Nothing)

        CurrentFlowsheet.Options.PropertyPackages.Add(pp.UniqueID, pp)
        Me.dgvpp.Rows.Add(New Object() {pp.UniqueID, pp.Tag, pp.ComponentName, "..."})
        Me.dgvpp.Rows(Me.dgvpp.Rows.Count - 1).Selected = True
        If TypeOf pp Is PropertyPackages.CAPEOPENPropertyPackage Then
            pp.DisplayEditingForm()
        End If
    End Sub

    Private Sub ComboBox2_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBox2.SelectedIndexChanged

        CurrentFlowsheet.Options.SelectedUnitSystem = FormMain.AvailableUnitSystems.Item(ComboBox2.SelectedItem.ToString)
        Dim su As SystemsOfUnits.Units = CurrentFlowsheet.Options.SelectedUnitSystem

        FormMain.AnalyticsProvider?.RegisterEvent("System of Units Selected", su.Name, Nothing)

        With Me.DataGridView1.Rows
            .Clear()
            .Add(New String() {DWSIM.App.GetLocalString("Temperatura"), su.temperature, DWSIM.App.GetLocalString("Presso"), su.pressure})
            .Add(New String() {DWSIM.App.GetLocalString("Vazomssica"), su.massflow, DWSIM.App.GetLocalString("Vazomolar"), su.molarflow})
            .Add(New String() {DWSIM.App.GetLocalString("Vazovolumtrica"), su.volumetricFlow, DWSIM.App.GetLocalString("EntalpiaEspecfica"), su.enthalpy})
            .Add(New String() {DWSIM.App.GetLocalString("EntropiaEspecfica"), su.entropy, DWSIM.App.GetLocalString("Massamolar"), su.molecularWeight})
            .Add(New String() {DWSIM.App.GetLocalString("Massaespecfica"), su.density, DWSIM.App.GetLocalString("Tensosuperficial"), su.surfaceTension})
            .Add(New String() {DWSIM.App.GetLocalString("CapacidadeCalorfica"), su.heatCapacityCp, DWSIM.App.GetLocalString("Condutividadetrmica"), su.thermalConductivity})
            .Add(New String() {DWSIM.App.GetLocalString("Viscosidadecinemtica"), su.cinematic_viscosity, DWSIM.App.GetLocalString("Viscosidadedinmica"), su.viscosity})
            .Add(New String() {DWSIM.App.GetLocalString("DeltaT2"), su.deltaT, DWSIM.App.GetLocalString("DeltaP"), su.deltaP})
            .Add(New String() {DWSIM.App.GetLocalString("ComprimentoHead"), su.head, DWSIM.App.GetLocalString("FluxodeEnergyFlow"), su.heatflow})
            .Add(New String() {DWSIM.App.GetLocalString("Tempo"), su.time, DWSIM.App.GetLocalString("Volume"), su.volume})
            .Add(New String() {DWSIM.App.GetLocalString("VolumeMolar"), su.molar_volume, DWSIM.App.GetLocalString("rea"), su.area})
            .Add(New String() {DWSIM.App.GetLocalString("DimetroEspessura"), su.diameter, DWSIM.App.GetLocalString("Fora"), su.force})
            .Add(New String() {DWSIM.App.GetLocalString("Aceleracao"), su.accel, DWSIM.App.GetLocalString("CoefdeTransfdeCalor"), su.heat_transf_coeff})
            .Add(New String() {DWSIM.App.GetLocalString("ConcMolar"), su.molar_conc, DWSIM.App.GetLocalString("ConcMssica"), su.mass_conc})
            .Add(New String() {DWSIM.App.GetLocalString("TaxadeReao"), su.reac_rate, DWSIM.App.GetLocalString("VolEspecfico"), su.spec_vol})
            .Add(New String() {DWSIM.App.GetLocalString("MolarEnthalpy"), su.molar_enthalpy, DWSIM.App.GetLocalString("MolarEntropy"), su.molar_entropy})
            .Add(New String() {DWSIM.App.GetLocalString("Velocity"), su.velocity, DWSIM.App.GetLocalString("HXFoulingFactor"), su.foulingfactor})
            .Add(New String() {DWSIM.App.GetLocalString("FilterSpecificCakeResistance"), su.cakeresistance, DWSIM.App.GetLocalString("FilterMediumResistance"), su.mediumresistance})
            .Add(New String() {DWSIM.App.GetLocalString("IsothermalCompressibility"), su.compressibility, DWSIM.App.GetLocalString("JouleThomsonCoefficient"), su.jouleThomsonCoefficient})
            .Add(New String() {DWSIM.App.GetLocalString("Conductance"), su.conductance, DWSIM.App.GetLocalString("DistComp"), su.distance})
            .Add(New String() {DWSIM.App.GetLocalString("Heat/Energy"), su.heat, DWSIM.App.GetLocalString("Mass"), su.mass})
            .Add(New String() {DWSIM.App.GetLocalString("Moles"), su.mole, Nothing, Nothing})
        End With

        If ComboBox2.SelectedIndex <= 3 Then
            Me.DataGridView1.Columns(1).ReadOnly = True
            Me.DataGridView1.Columns(3).ReadOnly = True
        Else
            Me.DataGridView1.Columns(1).ReadOnly = False
            Me.DataGridView1.Columns(3).ReadOnly = False
        End If

        '.Add(New Object() {DWSIM.App.GetLocalString("Temperatura")})
        With DirectCast(Me.DataGridView1.Rows.Item(0).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.temperature).ToArray())
            .Style.Tag = 1
            .Value = su.temperature
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Presso")})
        With DirectCast(Me.DataGridView1.Rows.Item(0).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.pressure).ToArray())
            .Style.Tag = 2
            .Value = su.pressure
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Vazomssica")})
        With DirectCast(Me.DataGridView1.Rows.Item(1).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.massflow).ToArray())
            .Style.Tag = 3
            .Value = su.massflow
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Vazomolar")})
        With DirectCast(Me.DataGridView1.Rows.Item(1).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.molarflow).ToArray())
            .Value = su.molarflow
            .Style.Tag = 4
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Vazovolumtrica")})
        With DirectCast(Me.DataGridView1.Rows.Item(2).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.volumetricFlow).ToArray())
            .Value = su.volumetricFlow
            .Style.Tag = 5
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("EntalpiaEspecfica")})
        With DirectCast(Me.DataGridView1.Rows.Item(2).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.enthalpy).ToArray())
            .Value = su.enthalpy
            .Style.Tag = 6
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("EntropiaEspecfica")})
        With DirectCast(Me.DataGridView1.Rows.Item(3).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.entropy).ToArray())
            .Value = su.entropy
            .Style.Tag = 7
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Massamolar")})
        With DirectCast(Me.DataGridView1.Rows.Item(3).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.molecularWeight).ToArray())
            .Value = su.molecularWeight
            .Style.Tag = 8
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Massaespecfica")})
        With DirectCast(Me.DataGridView1.Rows.Item(4).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.density).ToArray())
            .Value = su.density
            .Style.Tag = 10
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Tensosuperficial")})
        With DirectCast(Me.DataGridView1.Rows.Item(4).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.surfaceTension).ToArray())
            .Value = su.surfaceTension
            .Style.Tag = 9
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("CapacidadeCalorfica")})
        With DirectCast(Me.DataGridView1.Rows.Item(5).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.heatCapacityCp).ToArray())
            .Value = su.heatCapacityCp
            .Style.Tag = 11
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Condutividadetrmica")})
        With DirectCast(Me.DataGridView1.Rows.Item(5).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.thermalConductivity).ToArray())
            .Value = su.thermalConductivity
            .Style.Tag = 12
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Viscosidadecinemtica")})
        With DirectCast(Me.DataGridView1.Rows.Item(6).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.cinematic_viscosity).ToArray())
            .Value = su.cinematic_viscosity
            .Style.Tag = 13
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("ViscosidadeDinmica1")})
        With DirectCast(Me.DataGridView1.Rows.Item(6).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.viscosity).ToArray())
            .Value = su.viscosity
            .Style.Tag = 14
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("DeltaP")})
        With DirectCast(Me.DataGridView1.Rows.Item(7).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.deltaP).ToArray())
            .Value = su.deltaP
            .Style.Tag = 15
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("DeltaT2")})
        With DirectCast(Me.DataGridView1.Rows.Item(7).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.deltaT).ToArray())
            .Value = su.deltaT
            .Style.Tag = 16
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("ComprimentoHead")})
        With DirectCast(Me.DataGridView1.Rows.Item(8).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.head).ToArray())
            .Value = su.head
            .Style.Tag = 17
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("FluxodeEnergyFlow")})
        With DirectCast(Me.DataGridView1.Rows.Item(8).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.heatflow).ToArray())
            .Value = su.heatflow
            .Style.Tag = 18
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Tempo")})
        With DirectCast(Me.DataGridView1.Rows.Item(9).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.time).ToArray())
            .Value = su.time
            .Style.Tag = 19
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Volume")})
        With DirectCast(Me.DataGridView1.Rows.Item(9).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.volume).ToArray())
            .Value = su.volume
            .Style.Tag = 20
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("VolumeMolar")})
        With DirectCast(Me.DataGridView1.Rows.Item(10).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.molar_volume).ToArray())
            .Value = su.molar_volume
            .Style.Tag = 21
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("rea")})
        With DirectCast(Me.DataGridView1.Rows.Item(10).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.area).ToArray())
            .Value = su.area
            .Style.Tag = 22
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("DimetroEspessura")})
        With DirectCast(Me.DataGridView1.Rows.Item(11).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.diameter).ToArray())
            .Value = su.diameter
            .Style.Tag = 23
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Fora")})
        With DirectCast(Me.DataGridView1.Rows.Item(11).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.force).ToArray())
            .Value = su.force
            .Style.Tag = 24
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("CoefdeTransfdeCalor")})
        With DirectCast(Me.DataGridView1.Rows.Item(12).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.heat_transf_coeff).ToArray())
            .Value = su.heat_transf_coeff
            .Style.Tag = 25
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Aceleracao")})
        With DirectCast(Me.DataGridView1.Rows.Item(12).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.accel).ToArray())
            .Value = su.accel
            .Style.Tag = 26
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("ConcentraoMolar")})
        With DirectCast(Me.DataGridView1.Rows.Item(13).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.molar_conc).ToArray())
            .Value = su.molar_conc
            .Style.Tag = 28
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("ConcentraoMssica")})
        With DirectCast(Me.DataGridView1.Rows.Item(13).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.mass_conc).ToArray())
            .Value = su.mass_conc
            .Style.Tag = 29
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("TaxadeReao")})
        With DirectCast(Me.DataGridView1.Rows.Item(14).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.reac_rate).ToArray())
            .Value = su.reac_rate
            .Style.Tag = 30
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("VolumeEspecfico")})
        With DirectCast(Me.DataGridView1.Rows.Item(14).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.spec_vol).ToArray())
            .Value = su.spec_vol
            .Style.Tag = 27
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("MolarEnthalpy")})
        With DirectCast(Me.DataGridView1.Rows.Item(15).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.molar_enthalpy).ToArray())
            .Value = su.molar_enthalpy
            .Style.Tag = 31
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("MolarEntropy")})
        With DirectCast(Me.DataGridView1.Rows.Item(15).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.molar_entropy).ToArray())
            .Value = su.molar_entropy
            .Style.Tag = 32
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Velocity")})
        With DirectCast(Me.DataGridView1.Rows.Item(16).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.velocity).ToArray())
            .Value = su.velocity
            .Style.Tag = 33
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("HXFoulingFactor")})
        With DirectCast(Me.DataGridView1.Rows.Item(16).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.foulingfactor).ToArray())
            .Value = su.foulingfactor
            .Style.Tag = 34
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("FilterSpecificCakeResistance")})
        With DirectCast(Me.DataGridView1.Rows.Item(17).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.cakeresistance).ToArray())
            .Value = su.cakeresistance
            .Style.Tag = 35
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("FilterMediumResistance")})
        With DirectCast(Me.DataGridView1.Rows.Item(17).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.mediumresistance).ToArray())
            .Value = su.mediumresistance
            .Style.Tag = 36
        End With

        With DirectCast(Me.DataGridView1.Rows.Item(18).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.compressibility).ToArray())
            .Value = su.compressibility
            .Style.Tag = 37
        End With

        With DirectCast(Me.DataGridView1.Rows.Item(18).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.jouleThomsonCoefficient).ToArray())
            .Value = su.jouleThomsonCoefficient
            .Style.Tag = 38
        End With

        With DirectCast(Me.DataGridView1.Rows.Item(19).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.conductance).ToArray)
            .Value = su.conductance
            .Style.Tag = 39
        End With

        With DirectCast(Me.DataGridView1.Rows.Item(19).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.distance).ToArray)
            .Value = su.distance
            .Style.Tag = 40
        End With

        With DirectCast(Me.DataGridView1.Rows.Item(20).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.heat).ToArray)
            .Value = su.heat
            .Style.Tag = 41
        End With

        With DirectCast(Me.DataGridView1.Rows.Item(20).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.mass).ToArray)
            .Value = su.mass
            .Style.Tag = 42
        End With

        With DirectCast(Me.DataGridView1.Rows.Item(21).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.mole).ToArray)
            .Value = su.mole
            .Style.Tag = 43
        End With

    End Sub

    Private Sub DataGridView1_CellValueChanged1(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles DataGridView1.CellValueChanged

        If loaded Then

            Dim cell As DataGridViewCell = Me.DataGridView1.Rows(e.RowIndex).Cells(e.ColumnIndex)
            Dim oldvalue As String = ""
            Dim member As String = ""

            Dim su As SystemsOfUnits.Units = CurrentFlowsheet.Options.SelectedUnitSystem

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
                Case 39
                    member = "conductance"
                    oldvalue = su.conductance
                    su.conductance = cell.Value
                Case 40
                    member = "distance"
                    oldvalue = su.distance
                    su.distance = cell.Value
                Case 41
                    member = "heat"
                    oldvalue = su.heat
                    su.heat = cell.Value
                Case 42
                    oldvalue = su.mass
                    su.mass = cell.Value
                Case 43
                    oldvalue = su.mole
                    su.mole = cell.Value
            End Select

        End If

    End Sub


    Private Sub ListViewPP_DoubleClick(sender As Object, e As EventArgs) Handles DataGridViewPP.DoubleClick
        If Me.DataGridViewPP.SelectedRows.Count = 1 Then
            Button8.PerformClick()
        End If
    End Sub

    Private Sub ListViewPP_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DataGridViewPP.SelectionChanged
        If Me.DataGridViewPP.SelectedRows.Count > 0 Then
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
            Dim pp As PropertyPackages.PropertyPackage = CurrentFlowsheet.Options.PropertyPackages(ppid)
            If TypeOf pp Is CAPEOPENPropertyPackage Then
                pp.DisplayEditingForm()
            Else
                If pp.IsConfigurable Then
                    pp.DisplayGroupedEditingForm()
                Else
                    MessageBox.Show(DWSIM.App.GetLocalString("NonConfigurablePP"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                End If
            End If
        End If
    End Sub

    Private Sub btnInfoLeft_Click(sender As Object, e As EventArgs) Handles btnInfoLeft.Click

        If ogc1.SelectedRows.Count > 0 Then
            Dim f As New FormPureComp() With {.Flowsheet = CurrentFlowsheet, .Added = False, .MyCompound = Me.CurrentFlowsheet.AvailableCompounds(ogc1.SelectedRows(0).Cells(0).Value)}
            f.ShowDialog(Me)
        Else
            MessageBox.Show(DWSIM.App.GetLocalString("Error! No component selected!"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End If

    End Sub

    Private Sub btnCloneUnits_Click(sender As Object, e As EventArgs) Handles btnCloneUnits.Click

        Dim newsu = New SystemsOfUnits.Units

        newsu = Newtonsoft.Json.JsonConvert.DeserializeObject(Of SharedClasses.SystemsOfUnits.Units)(Newtonsoft.Json.JsonConvert.SerializeObject(CurrentFlowsheet.Options.SelectedUnitSystem))
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

            UpdateAddedList()

        End If

    End Sub

    Private Sub UpdateAddedList()

        Dim added As String = ""
        For Each c In CurrentFlowsheet.Options.SelectedComponents.Values
            added += c.Name + ", "
        Next
        added = added.TrimEnd()
        added = added.TrimEnd(",")
        txtAdded.Text = added

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
        FormMain.AnalyticsProvider?.RegisterEvent("Importing Compounds from Online Sources", "", Nothing)
        Dim f As New FormImportCompoundOnline
        If f.ShowDialog(Me) = DialogResult.OK Then
            Try
                Dim comp = f.BaseCompound
                If Not Me.CurrentFlowsheet.AvailableCompounds.ContainsKey(comp.Name) Then
                    Me.CurrentFlowsheet.AvailableCompounds.Add(comp.Name, comp)
                    Me.CurrentFlowsheet.Options.SelectedComponents.Add(comp.Name, comp)
                    Me.CurrentFlowsheet.Options.NotSelectedComponents.Remove(comp.Name)
                    Dim ms As Streams.MaterialStream
                    Dim proplist As New ArrayList
                    For Each ms In CurrentFlowsheet.Collections.FlowsheetObjectCollection.Values
                        For Each phase As BaseClasses.Phase In ms.Phases.Values
                            phase.Compounds.Add(comp.Name, New BaseClasses.Compound(comp.Name, ""))
                            phase.Compounds(comp.Name).ConstantProperties = comp
                        Next
                    Next
                    ogc1.Rows.Add(New Object() {comp.Name, True, comp.Name, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
                    ogc1.Sort(colAdd, System.ComponentModel.ListSortDirection.Descending)

                    UpdateAddedList()

                Else
                    MessageBox.Show(DWSIM.App.GetLocalString("CompoundExists"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End If
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If
    End Sub

    Private Sub ToolStripMenuItem2_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem2.Click

        FormMain.AnalyticsProvider?.RegisterEvent("Importing Compounds from JSON Files", "", Nothing)

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowOpenDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON File", "*.json")})

        If handler IsNot Nothing Then
            Dim jsondata = handler.ReadAllText()
            Try
                Dim comp = Newtonsoft.Json.JsonConvert.DeserializeObject(Of BaseClasses.ConstantProperties)(jsondata)
                If Not Me.CurrentFlowsheet.AvailableCompounds.ContainsKey(comp.Name) Then
                    Me.CurrentFlowsheet.AvailableCompounds.Add(comp.Name, comp)
                    Me.CurrentFlowsheet.Options.SelectedComponents.Add(comp.Name, comp)
                    Me.CurrentFlowsheet.Options.NotSelectedComponents.Remove(comp.Name)
                    Dim ms As Streams.MaterialStream
                    Dim proplist As New ArrayList
                    For Each ms In CurrentFlowsheet.Collections.FlowsheetObjectCollection.Values
                        For Each phase As BaseClasses.Phase In ms.Phases.Values
                            phase.Compounds.Add(comp.Name, New BaseClasses.Compound(comp.Name, ""))
                            phase.Compounds(comp.Name).ConstantProperties = comp
                        Next
                    Next
                    ogc1.Rows.Add(New Object() {comp.Name, True, comp.Name, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
                    ogc1.Sort(colAdd, System.ComponentModel.ListSortDirection.Descending)
                    UpdateAddedList()
                Else
                    MessageBox.Show(DWSIM.App.GetLocalString("CompoundExists"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End If
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
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

        If CurrentFlowsheet.Options.PropertyPackages.Count = 0 Then

            Dim pp As New PropertyPackages.PengRobinsonPropertyPackage
            With pp
                pp.Tag = pp.ComponentName + " (" + (CurrentFlowsheet.PropertyPackages.Count + 1).ToString() + ")"
                pp.UniqueID = "PP-" & Guid.NewGuid.ToString
                pp.Flowsheet = CurrentFlowsheet
            End With
            CurrentFlowsheet.Options.PropertyPackages.Add(pp.UniqueID, pp)

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
            NewMDIChild.MdiParent = CurrentFlowsheet.MdiParent
            'Display the new form.
            NewMDIChild.Text = "CompCreator" & FormMain.m_childcount
            NewMDIChild.Show()
            FormMain.m_childcount += 1

        End If

    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs)
        txtSearch.Text = ""
    End Sub

    Private Sub ImportFromThermoChemicalsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ImportFromThermoChemicalsToolStripMenuItem.Click

        FormMain.AnalyticsProvider?.RegisterEvent("Importing Compounds from Thermo/Chemicals", "", Nothing)

        Dim f As New FormImportCompoundFromThermo
        If f.ShowDialog(Me) = DialogResult.OK Then
            Try
                Dim comp = f.compdata
                If Not Me.CurrentFlowsheet.AvailableCompounds.ContainsKey(comp.Name) Then
                    Me.CurrentFlowsheet.AvailableCompounds.Add(comp.Name, comp)
                    Me.CurrentFlowsheet.Options.SelectedComponents.Add(comp.Name, comp)
                    Me.CurrentFlowsheet.Options.NotSelectedComponents.Remove(comp.Name)
                    Dim ms As Streams.MaterialStream
                    Dim proplist As New ArrayList
                    For Each ms In CurrentFlowsheet.Collections.FlowsheetObjectCollection.Values.Where(Function(obj) obj.GraphicObject.ObjectType = ObjectType.MaterialStream)
                        For Each phase As BaseClasses.Phase In ms.Phases.Values
                            phase.Compounds.Add(comp.Name, New BaseClasses.Compound(comp.Name, ""))
                            phase.Compounds(comp.Name).ConstantProperties = comp
                        Next
                    Next
                    ogc1.Rows.Add(New Object() {comp.Name, True, comp.Name, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
                    ogc1.Sort(colAdd, System.ComponentModel.ListSortDirection.Descending)

                    UpdateAddedList()

                Else
                    MessageBox.Show(DWSIM.App.GetLocalString("CompoundExists"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End If
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If
    End Sub

    Private Sub rbVLE_CheckedChanged(sender As Object, e As EventArgs) Handles rbVLE.CheckedChanged, rbVLLE.CheckedChanged, rbNoFlash.CheckedChanged, rbSVLLE.CheckedChanged

        If loaded Then
            For Each pp In CurrentFlowsheet.Options.PropertyPackages.Values
                If rbVLE.Checked Then
                    pp.FlashSettings(Interfaces.Enums.FlashSetting.ForceEquilibriumCalculationType) = "VLE"
                ElseIf rbVLLE.Checked Then
                    pp.FlashSettings(Interfaces.Enums.FlashSetting.ForceEquilibriumCalculationType) = "VLLE"
                ElseIf rbSVLLE.Checked Then
                    pp.FlashSettings(Interfaces.Enums.FlashSetting.ForceEquilibriumCalculationType) = "Default"
                Else
                    pp.FlashSettings(Interfaces.Enums.FlashSetting.ForceEquilibriumCalculationType) = "NoFlash"
                End If
            Next
        End If

    End Sub

    Public Sub SetupPPRecommendations()

        Dim imgOK = New Bitmap(My.Resources.checkmark_48px, New Size(24 * Settings.DpiScale, 24 * Settings.DpiScale))
        Dim imgCaution = New Bitmap(My.Resources.box_important_48px, New Size(24 * Settings.DpiScale, 24 * Settings.DpiScale))

        Dim names = CurrentFlowsheet.SelectedCompounds.Keys.Select(Function(n) n.ToLower()).ToList()

        Dim hasCP = CurrentFlowsheet.SelectedCompounds.Values.Where(Function(c) c.IsCOOLPROPSupported).Count()

        Dim elecs = CurrentFlowsheet.SelectedCompounds.Values.Where(Function(c) c.IsSalt Or c.IsIon Or c.IsHydratedSalt).Count()

        If elecs > 0 Then
            'contains electrolytes
            rbSVLLE.Checked = True
            For Each row As DataGridViewRow In DataGridViewPP.Rows
                If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                    Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                    If pp.IsElectrolytePP Then
                        row.Cells(1).Value = 1
                        row.Cells(2).Value = imgOK
                        If pp.GetType().ToString().Contains("ProExtensions") Then
                            ChangeRowForeColor(row, Color.DarkGreen)
                        Else
                            ChangeRowForeColor(row, Color.Blue)
                        End If
                    Else
                        row.Cells(1).Value = 0
                        row.Cells(2).Value = imgCaution
                        ChangeRowForeColor(row, Color.DarkGray)
                    End If
                Else
                    row.Cells(1).Value = 0
                    row.Cells(2).Value = imgCaution
                    ChangeRowForeColor(row, Color.DarkGray)
                End If
            Next
        ElseIf names.Contains("water") And names.Where(Function(x) x.EndsWith("ane") Or x.EndsWith("ene") Or x.EndsWith("ine")).Count > 0 Then
            'Water + Hydrocarbons
            rbSVLLE.Checked = True
            For Each row As DataGridViewRow In DataGridViewPP.Rows
                If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                    Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                    Select Case pp.PackageType
                        Case PackageType.EOS, PackageType.CorrespondingStates
                            row.Cells(1).Value = 1
                            row.Cells(2).Value = imgOK
                            If pp.GetType().ToString().Contains("ProExtensions") Then
                                ChangeRowForeColor(row, Color.DarkGreen)
                            Else
                                ChangeRowForeColor(row, Color.Blue)
                            End If
                        Case Else
                            row.Cells(1).Value = 0
                            row.Cells(2).Value = imgCaution
                            ChangeRowForeColor(row, Color.DarkGray)
                    End Select
                Else
                    Dim ptype = row.Cells(0).Value
                    Select Case ptype
                        Case PackageType.EOS, PackageType.CorrespondingStates
                            row.Cells(1).Value = 1
                            row.Cells(2).Value = imgOK
                            ChangeRowForeColor(row, Color.DarkGreen)
                        Case Else
                            row.Cells(1).Value = 0
                            row.Cells(2).Value = imgCaution
                            ChangeRowForeColor(row, Color.DarkGray)
                    End Select
                End If
            Next
        ElseIf names.Where(Function(x) x.EndsWith("al")).Count > 0 And names.Where(Function(x) x.Contains("ane") Or x.Contains("ene") Or x.Contains("ine")).Count > 0 Then
            'Aldehydes + Hydrocarbons
            rbSVLLE.Checked = True
            For Each row As DataGridViewRow In DataGridViewPP.Rows
                If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                    Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                    Select Case pp.PackageType
                        Case PackageType.ActivityCoefficient, PackageType.CorrespondingStates
                            row.Cells(1).Value = 1
                            row.Cells(2).Value = imgOK
                            If pp.GetType().ToString().Contains("ProExtensions") Then
                                ChangeRowForeColor(row, Color.DarkGreen)
                            Else
                                ChangeRowForeColor(row, Color.Blue)
                            End If
                        Case Else
                            row.Cells(1).Value = 0
                            row.Cells(2).Value = imgCaution
                            ChangeRowForeColor(row, Color.DarkGray)
                    End Select
                Else
                    Dim ptype = row.Cells(0).Value
                    Select Case ptype
                        Case PackageType.ActivityCoefficient, PackageType.CorrespondingStates
                            row.Cells(1).Value = 1
                            row.Cells(2).Value = imgOK
                            ChangeRowForeColor(row, Color.DarkGreen)
                        Case Else
                            row.Cells(1).Value = 0
                            row.Cells(2).Value = imgCaution
                            ChangeRowForeColor(row, Color.DarkGray)
                    End Select
                End If
            Next
        ElseIf names.Where(Function(x) x.EndsWith("ol")).Count > 0 And names.Where(Function(x) x.EndsWith("ane") Or x.EndsWith("ene") Or x.EndsWith("ine")).Count > 0 Then
            'Alcohols + Hydrocarbons
            rbSVLLE.Checked = True
            For Each row As DataGridViewRow In DataGridViewPP.Rows
                If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                    Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                    Select Case pp.PackageType
                        Case PackageType.ActivityCoefficient, PackageType.CorrespondingStates
                            row.Cells(1).Value = 1
                            row.Cells(2).Value = imgOK
                            If pp.GetType().ToString().Contains("ProExtensions") Then
                                ChangeRowForeColor(row, Color.DarkGreen)
                            Else
                                ChangeRowForeColor(row, Color.Blue)
                            End If
                        Case Else
                            row.Cells(1).Value = 0
                            row.Cells(2).Value = imgCaution
                            ChangeRowForeColor(row, Color.DarkGray)
                    End Select
                Else
                    Dim ptype = row.Cells(0).Value
                    Select Case ptype
                        Case PackageType.ActivityCoefficient, PackageType.CorrespondingStates
                            row.Cells(1).Value = 1
                            row.Cells(2).Value = imgOK
                            ChangeRowForeColor(row, Color.DarkGreen)
                        Case Else
                            row.Cells(1).Value = 0
                            row.Cells(2).Value = imgCaution
                            ChangeRowForeColor(row, Color.DarkGray)
                    End Select
                End If
            Next
        ElseIf names.Contains("water") And names.Where(Function(x) x.EndsWith("ol")).Count > 0 Then
            'Water + C4+ Alcohols
            rbSVLLE.Checked = True
            For Each row As DataGridViewRow In DataGridViewPP.Rows
                If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                    Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                    Select Case pp.PackageType
                        Case PackageType.ActivityCoefficient, PackageType.CorrespondingStates
                            row.Cells(1).Value = 1
                            row.Cells(2).Value = imgOK
                            If pp.GetType().ToString().Contains("ProExtensions") Then
                                ChangeRowForeColor(row, Color.DarkGreen)
                            Else
                                ChangeRowForeColor(row, Color.Blue)
                            End If
                        Case Else
                            row.Cells(1).Value = 0
                            row.Cells(2).Value = imgCaution
                            ChangeRowForeColor(row, Color.DarkGray)
                    End Select
                    If pp.DisplayName.Contains("Strÿjek-Vera") Then
                        row.Cells(1).Value = 1
                        row.Cells(2).Value = imgOK
                        ChangeRowForeColor(row, Color.Blue)
                    End If
                Else
                    Dim ptype = row.Cells(0).Value
                    Select Case ptype
                        Case PackageType.ActivityCoefficient, PackageType.CorrespondingStates
                            row.Cells(1).Value = 1
                            row.Cells(2).Value = imgOK
                            ChangeRowForeColor(row, Color.DarkGreen)
                        Case Else
                            row.Cells(1).Value = 0
                            row.Cells(2).Value = imgCaution
                            ChangeRowForeColor(row, Color.DarkGray)
                    End Select
                End If
            Next
        ElseIf names.Where(Function(x) x.EndsWith("ane") Or x.EndsWith("ene") Or x.EndsWith("ine")).Count > 0 Then
            'Hydrocarbons
            rbVLE.Checked = True
            For Each row As DataGridViewRow In DataGridViewPP.Rows
                If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                    Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                    Select Case pp.PackageType
                        Case PackageType.EOS, PackageType.CorrespondingStates, PackageType.VaporPressure
                            row.Cells(1).Value = 1
                            row.Cells(2).Value = imgOK
                            If pp.GetType().ToString().Contains("ProExtensions") Then
                                ChangeRowForeColor(row, Color.DarkGreen)
                            Else
                                ChangeRowForeColor(row, Color.Blue)
                            End If
                        Case Else
                            row.Cells(1).Value = 0
                            row.Cells(2).Value = imgCaution
                            ChangeRowForeColor(row, Color.DarkGray)
                    End Select
                Else
                    Dim ptype = row.Cells(0).Value
                    Select Case ptype
                        Case PackageType.EOS, PackageType.CorrespondingStates, PackageType.VaporPressure
                            row.Cells(1).Value = 1
                            row.Cells(2).Value = imgOK
                            ChangeRowForeColor(row, Color.DarkGreen)
                        Case Else
                            row.Cells(1).Value = 0
                            row.Cells(2).Value = imgCaution
                            ChangeRowForeColor(row, Color.DarkGray)
                    End Select
                End If
            Next
        ElseIf names.Where(Function(x) x.EndsWith("ol")).Count > 0 Then
            'Alcohols 
            rbVLE.Checked = True
            For Each row As DataGridViewRow In DataGridViewPP.Rows
                If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                    Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                    Select Case pp.PackageType
                        Case PackageType.ActivityCoefficient, PackageType.CorrespondingStates, PackageType.VaporPressure
                            row.Cells(1).Value = 1
                            row.Cells(2).Value = imgOK
                            If pp.GetType().ToString().Contains("ProExtensions") Then
                                ChangeRowForeColor(row, Color.DarkGreen)
                            Else
                                ChangeRowForeColor(row, Color.Blue)
                            End If
                        Case Else
                            row.Cells(1).Value = 0
                            row.Cells(2).Value = imgCaution
                            ChangeRowForeColor(row, Color.DarkGray)
                    End Select
                Else
                    Dim ptype = row.Cells(0).Value
                    Select Case ptype
                        Case PackageType.ActivityCoefficient, PackageType.CorrespondingStates, PackageType.VaporPressure
                            row.Cells(1).Value = 1
                            row.Cells(2).Value = imgOK
                            ChangeRowForeColor(row, Color.DarkGreen)
                        Case Else
                            row.Cells(1).Value = 0
                            row.Cells(2).Value = imgCaution
                            ChangeRowForeColor(row, Color.DarkGray)
                    End Select
                End If
            Next
        ElseIf names.Where(Function(x) x.EndsWith("al")).Count > 0 Then
            'Aldehydes
            rbVLE.Checked = True
            For Each row As DataGridViewRow In DataGridViewPP.Rows
                If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                    Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                    Select Case pp.PackageType
                        Case PackageType.ActivityCoefficient, PackageType.CorrespondingStates, PackageType.VaporPressure
                            row.Cells(1).Value = 1
                            row.Cells(2).Value = imgOK
                            If pp.GetType().ToString().Contains("ProExtensions") Then
                                ChangeRowForeColor(row, Color.DarkGreen)
                            Else
                                ChangeRowForeColor(row, Color.Blue)
                            End If
                        Case Else
                            row.Cells(1).Value = 0
                            row.Cells(2).Value = imgCaution
                            ChangeRowForeColor(row, Color.DarkGray)
                    End Select
                Else
                    Dim ptype = row.Cells(0).Value
                    Select Case ptype
                        Case PackageType.ActivityCoefficient, PackageType.CorrespondingStates, PackageType.VaporPressure
                            row.Cells(1).Value = 1
                            row.Cells(2).Value = imgOK
                            ChangeRowForeColor(row, Color.DarkGreen)
                        Case Else
                            row.Cells(1).Value = 0
                            row.Cells(2).Value = imgCaution
                            ChangeRowForeColor(row, Color.DarkGray)
                    End Select
                End If
            Next
        Else
            rbSVLLE.Checked = True
            For Each row As DataGridViewRow In DataGridViewPP.Rows
                If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                    row.Cells(1).Value = 1
                    row.Cells(2).Value = imgOK
                    ChangeRowForeColor(row, Color.Blue)
                Else
                    row.Cells(1).Value = 1
                    row.Cells(2).Value = imgOK
                    ChangeRowForeColor(row, Color.DarkGreen)
                End If
            Next
        End If

        If names.Contains("water") Or names.Where(Function(x) x.EndsWith("ane") Or x.EndsWith("ene") Or x.EndsWith("ine")).Count > 0 Then
            'Water + Hydrocarbons
            rbSVLLE.Checked = True
            For Each row As DataGridViewRow In DataGridViewPP.Rows
                If row.Cells(4).Value.ToString().Contains("Petroleum Industry") Then
                    row.Cells(1).Value = 1
                    row.Cells(2).Value = imgOK
                    ChangeRowForeColor(row, Color.Blue)
                End If
            Next
        End If

        If hasCP > 0 Then
            For Each row As DataGridViewRow In DataGridViewPP.Rows
                If row.Cells(4).Value.ToString().Contains("CoolProp") Or
                        row.Cells(4).Value.ToString().Contains("REFPROP") Or
                        row.Cells(4).Value.ToString().Contains("Raoult") Then
                    row.Cells(1).Value = 1
                    row.Cells(2).Value = imgOK
                    ChangeRowForeColor(row, Color.Blue)
                End If
            Next
        End If

        If names.Contains("hydrogen") Then
            For Each row As DataGridViewRow In DataGridViewPP.Rows
                If row.Cells(4).Value.ToString().Contains("Streed") Or row.Cells(4).Value.ToString().Contains("1978") Then
                    row.Cells(1).Value = 1
                    row.Cells(2).Value = imgOK
                    ChangeRowForeColor(row, Color.Blue)
                End If
            Next
        End If

        If names.Contains("water") And names.Count = 1 Then
            For Each row As DataGridViewRow In DataGridViewPP.Rows
                If row.Cells(4).Value.ToString().Contains("Steam") Or
                    row.Cells(4).Value.ToString().Equals("CoolProp") Or
                    row.Cells(4).Value.ToString().Contains("REFPROP") Or
                    row.Cells(4).Value.ToString().Equals("Extended CoolProp") Or
                    row.Cells(4).Value.ToString().Contains("Raoult") Then
                    row.Cells(1).Value = 1
                    row.Cells(2).Value = imgOK
                    ChangeRowForeColor(row, Color.Blue)
                Else
                    row.Cells(1).Value = 0
                    row.Cells(2).Value = imgCaution
                    ChangeRowForeColor(row, Color.DarkGray)
                End If
            Next
        ElseIf names.Count = 1 Then
            For Each row As DataGridViewRow In DataGridViewPP.Rows
                If row.Cells(4).Value.ToString().Equals("CoolProp") Or
                    row.Cells(4).Value.ToString().Equals("Extended CoolProp") Or
                    row.Cells(4).Value.ToString().Contains("Raoult") Then
                    row.Cells(1).Value = 1
                    row.Cells(2).Value = imgOK
                    ChangeRowForeColor(row, Color.Blue)
                ElseIf Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                    Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                    Select Case pp.PackageType
                        Case PackageType.CorrespondingStates, PackageType.EOS
                            row.Cells(1).Value = 1
                            row.Cells(2).Value = imgOK
                            If pp.GetType().ToString().Contains("ProExtensions") Then
                                ChangeRowForeColor(row, Color.DarkGreen)
                            Else
                                ChangeRowForeColor(row, Color.Blue)
                            End If
                        Case Else
                            row.Cells(1).Value = 0
                            row.Cells(2).Value = imgCaution
                            ChangeRowForeColor(row, Color.DarkGray)
                    End Select
                Else
                    Dim ptype = row.Cells(0).Value
                    Select Case ptype
                        Case PackageType.CorrespondingStates, PackageType.EOS
                            row.Cells(1).Value = 1
                            row.Cells(2).Value = imgOK
                            ChangeRowForeColor(row, Color.DarkGreen)
                        Case Else
                            row.Cells(1).Value = 0
                            row.Cells(2).Value = imgCaution
                            ChangeRowForeColor(row, Color.DarkGray)
                    End Select
                End If
            Next
        End If

        If (names.Contains("carbon dioxide") Or names.Contains("hydrogen sulfide")) And
            (names.Contains("monoethanolamine") Or names.Contains("methyl diethanolamine") Or names.Contains("diethanolamine") Or names.Contains("piperazine")) Then
            'amines
            For Each row As DataGridViewRow In DataGridViewPP.Rows
                If row.Cells(4).Value.ToString().Contains("Amines") Then
                    row.Cells(1).Value = 1
                    row.Cells(2).Value = imgOK
                    ChangeRowForeColor(row, Color.Blue)
                Else
                    row.Cells(1).Value = 0
                    row.Cells(2).Value = imgCaution
                    ChangeRowForeColor(row, Color.DarkGray)
                End If
            Next
        End If

        DataGridViewPP.Sort(DataGridViewPP.Columns(4), System.ComponentModel.ListSortDirection.Ascending)
        DataGridViewPP.Sort(DataGridViewPP.Columns(1), System.ComponentModel.ListSortDirection.Descending)

        DataGridViewPP.FirstDisplayedScrollingRowIndex = DataGridViewPP.Rows.GetFirstRow(DataGridViewElementStates.Visible)

    End Sub

    Private Sub ChangeRowForeColor(row As DataGridViewRow, color As Color)

        For Each cell As DataGridViewCell In row.Cells
            cell.Style.ForeColor = color
        Next

    End Sub

    Private Sub WizardPage2_Commit(sender As Object, e As AeroWizard.WizardPageConfirmEventArgs) Handles WizardPage2.Commit

        If CurrentFlowsheet.SelectedCompounds.Count = 0 Then

            Dim t1 = CurrentFlowsheet.GetTranslatedString1("Please add at least one compound to proceed.")
            Dim t2 = CurrentFlowsheet.GetTranslatedString1("Error")

            MessageBox.Show(t1, t2, MessageBoxButtons.OK, MessageBoxIcon.Error)

            e.Cancel = True

        Else

            SetupPPRecommendations()

        End If

    End Sub

    Private Sub Button5_Click_1(sender As Object, e As EventArgs) Handles Button5.Click

        txtSearch.Text = ""

    End Sub

    Private Sub FormSimulWizard_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        FormMain.TranslateFormFunction?.Invoke(Me)
    End Sub

    Private Sub FormSimulWizard_Closing(sender As Object, e As CancelEventArgs) Handles Me.Closing

        FormMain.AnalyticsProvider?.RegisterEvent("Number of Compounds", CurrentFlowsheet.SelectedCompounds.Count, Nothing)

        FormMain.AnalyticsProvider?.RegisterEvent("Number of Property Packages", CurrentFlowsheet.PropertyPackages.Count, Nothing)

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click

        If DataGridViewPP.SelectedRows.Count > 0 Then
            If Integer.TryParse(Me.DataGridViewPP.SelectedRows(0).Cells(0).Value, New Integer) Then
                MessageBox.Show("This Property Package is available on DWSIM Pro.", "DWSIM Pro", MessageBoxButtons.OK, MessageBoxIcon.Information)
            Else
                Dim pp = FormMain.PropertyPackages(Me.DataGridViewPP.SelectedRows(0).Cells(0).Value)
                Dim fppi As New FormPropertyPackageInfo With {.PP = pp}
                fppi.ShowDialog()
            End If
        End If

    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click

        Process.Start("https://dwsim.org/wiki/index.php?title=Property_Package_Selection")

    End Sub

    Private Sub cbPPFilter_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPPFilter.SelectedIndexChanged

        Select Case cbPPFilter.SelectedIndex
            Case 0 'Most Popular
                For Each row As DataGridViewRow In DataGridViewPP.Rows
                    If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                        Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                        row.Visible = pp.Popular
                    Else
                        row.Visible = True
                    End If
                Next
            Case 1 'All Types
                For Each row As DataGridViewRow In DataGridViewPP.Rows
                    row.Visible = True
                Next
            Case 2 'Equations of State
                For Each row As DataGridViewRow In DataGridViewPP.Rows
                    If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                        Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                        row.Visible = If(pp.PackageType = PackageType.EOS, True, False)
                    Else
                        row.Visible = If(row.Cells(0).Value = PackageType.EOS, True, False)
                    End If
                Next
            Case 3 'Activity Coefficient
                For Each row As DataGridViewRow In DataGridViewPP.Rows
                    If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                        Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                        row.Visible = If(pp.PackageType = PackageType.ActivityCoefficient, True, False)
                    Else
                        row.Visible = If(row.Cells(0).Value = PackageType.ActivityCoefficient, True, False)
                    End If
                Next
            Case 4 'Vapor Pressure
                For Each row As DataGridViewRow In DataGridViewPP.Rows
                    If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                        Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                        row.Visible = If(pp.PackageType = PackageType.VaporPressure, True, False)
                    Else
                        row.Visible = If(row.Cells(0).Value = PackageType.VaporPressure, True, False)
                    End If
                Next
            Case 5 'Corresponding States
                For Each row As DataGridViewRow In DataGridViewPP.Rows
                    If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                        Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                        row.Visible = If(pp.PackageType = PackageType.CorrespondingStates, True, False)
                    Else
                        row.Visible = If(row.Cells(0).Value = PackageType.CorrespondingStates, True, False)
                    End If
                Next
            Case 6 'Specialized Models
                For Each row As DataGridViewRow In DataGridViewPP.Rows
                    If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                        Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                        row.Visible = If(pp.PackageType = PackageType.Specialized, True, False)
                    Else
                        row.Visible = If(row.Cells(0).Value = PackageType.Specialized, True, False)
                    End If
                Next
            Case 7 'Miscelaneous
                For Each row As DataGridViewRow In DataGridViewPP.Rows
                    If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                        Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                        row.Visible = If(pp.PackageType = PackageType.Miscelaneous Or
                            pp.PackageType = PackageType.CAPEOPEN Or
                            pp.PackageType = PackageType.ChaoSeader, True, False)
                    Else
                        row.Visible = If(row.Cells(0).Value = PackageType.Miscelaneous, True, False)
                    End If
                Next
            Case 8 'Electrolytes
                For Each row As DataGridViewRow In DataGridViewPP.Rows
                    If Integer.TryParse(row.Cells(0).Value, New Integer) = False Then
                        Dim pp = FormMain.PropertyPackages(row.Cells(0).Value)
                        row.Visible = If(pp.PackageType = PackageType.Electrolytes, True, False)
                    Else
                        row.Visible = If(row.Cells(0).Value = PackageType.Electrolytes, True, False)
                    End If
                Next
        End Select

    End Sub

    Private Sub CheckBox2_CheckedChanged(sender As Object, e As EventArgs) Handles chkActivateSmartObjectSolving.CheckedChanged

        If loaded Then
            CurrentFlowsheet.Options.ForceObjectSolving = Not chkActivateSmartObjectSolving.Checked
            FormMain.AnalyticsProvider?.RegisterEvent("Smart Object Solver Enabled", Not CurrentFlowsheet.Options.ForceObjectSolving, Nothing)
        End If

    End Sub

    Private Sub chkEnableFailSafeFlash_CheckedChanged(sender As Object, e As EventArgs) Handles chkEnableFailSafeFlash.CheckedChanged

        If loaded Then
            For Each pp As PropertyPackage In CurrentFlowsheet.PropertyPackages.Values
                Dim fs = pp.FlashSettings
                If chkEnableFailSafeFlash.Checked Then
                    fs(FlashSetting.FailSafeCalculationMode) = 1
                Else
                    fs(FlashSetting.FailSafeCalculationMode) = 3
                End If
            Next
        End If

    End Sub

    Private Sub chkDoubleClickToOpenEditors_CheckedChanged(sender As Object, e As EventArgs) Handles chkDoubleClickToOpenEditors.CheckedChanged

        If loaded Then

            My.Settings.DoubleClickToEdit = chkDoubleClickToOpenEditors.Checked

            GlobalSettings.Settings.EditOnSelect = Not My.Settings.DoubleClickToEdit

            FormMain.AnalyticsProvider?.RegisterEvent("Double-Click Editing Enabled", My.Settings.DoubleClickToEdit, Nothing)

        End If

    End Sub

    Private Sub StepWizardControl1_SelectedPageChanged(sender As Object, e As EventArgs) Handles StepWizardControl1.SelectedPageChanged

        FormMain.TranslateFormFunction?.Invoke(Me)

        If StepWizardControl1.SelectedPage Is WizardPage2 Then

            txtSearch.Focus()
            ActiveControl = txtSearch

        End If

    End Sub

    Private Sub FormSimulWizard_FormClosed(sender As Object, e As FormClosedEventArgs) Handles Me.FormClosed

        WizardFinished?.Invoke(StepWizardControl1, CurrentFlowsheet)
        WizardFinished2?.Invoke(StepWizardControl1, CurrentFlowsheet)
        WizardFinished3?.Invoke(StepWizardControl1, CurrentFlowsheet)

    End Sub

    Private Sub WizardPage3_Commit(sender As Object, e As WizardPageConfirmEventArgs) Handles WizardPage3.Commit

        If CurrentFlowsheet.PropertyPackages.Count = 0 Then

            Dim t1 = CurrentFlowsheet.GetTranslatedString1("Please add at least one property package to proceed.")
            Dim t2 = CurrentFlowsheet.GetTranslatedString1("Error")

            MessageBox.Show(t1, t2, MessageBoxButtons.OK, MessageBoxIcon.Error)

            e.Cancel = True

        End If

    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles chkEnableUndoRedo.CheckedChanged

        If loaded Then

            CurrentFlowsheet.Options.EnabledUndoRedo = chkEnableUndoRedo.Checked

            FormMain.AnalyticsProvider?.RegisterEvent("Undo/Redo Enabled/Disabled", CurrentFlowsheet.Options.EnabledUndoRedo, Nothing)

        End If

    End Sub

End Class
