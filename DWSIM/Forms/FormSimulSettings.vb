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

Imports System.Xml.Serialization
Imports DWSIM.Thermodynamics.BaseClasses
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Runtime.Serialization.Formatters
Imports System.IO
Imports DWSIM.FlowsheetSolver
Imports System.Linq
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.Simulate365.FormFactories
Imports DWSIM.Simulate365.Models
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker
Imports AeroWizard

Public Class FormSimulSettings

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public CurrentFlowsheet As FormFlowsheet
    Public loaded As Boolean = False
    Public initialized As Boolean = False
    Public supports As Boolean = True

    Private availableproperties As New Dictionary(Of String, String())
    Private aTypeList As New List(Of Type)
    Private aTypeRefs As New Dictionary(Of String, String)

    Private prevsort As System.ComponentModel.ListSortDirection = System.ComponentModel.ListSortDirection.Ascending
    Private prevcol As Integer = 1

    Private CompoundList As List(Of String)
    Private Indexes As Dictionary(Of String, Integer)

    Dim vdPP, vdSR As MessageBox()

    Dim SetHeights As Boolean = False

    Public Shared AddMoreTabs As Action(Of TabControl, IFlowsheet)

    Private Sub FormStSim_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Me.Load

        Dim sw As Integer = Screen.PrimaryScreen.Bounds.Width
        Dim sh As Integer = Screen.PrimaryScreen.Bounds.Height
        Dim w = 900 * GlobalSettings.Settings.DpiScale
        Dim h = 600 * GlobalSettings.Settings.DpiScale

        If Me.Pane IsNot Nothing Then
            Me.Pane.FloatWindow.SetBounds((sw - w) / 2, (sh - h) / 2, w, h)
        End If

        Me.TabText = Me.Text

        initialized = True

        If DWSIM.App.IsRunningOnMono Then
            Me.ogc1.SelectionMode = DataGridViewSelectionMode.CellSelect
            Me.dgvpp.SelectionMode = DataGridViewSelectionMode.CellSelect
        End If

        Dim rm As New FormReacManager
        rm.CurrentFlowsheet = CurrentFlowsheet
        rm.Dock = DockStyle.Fill
        TabPageReactions.Controls.Add(rm)

        DataGridViewPP.Columns(1).Width = 24 * Settings.DpiScale

        Init()

        AddMoreTabs?.Invoke(TabControl1, CurrentFlowsheet)

    End Sub

    Private Sub FormStSim_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing

        If CurrentFlowsheet IsNot Nothing Then
            If Me.CurrentFlowsheet.Options.SelectedComponents.Count = 0 And Me.CurrentFlowsheet.Options.PropertyPackages.Count = 0 Then
                MessageBox.Show(DWSIM.App.GetLocalString("Adicionesubstnciassi"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                MessageBox.Show(DWSIM.App.GetLocalString("NoexistemPacotesdePr"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            ElseIf Me.CurrentFlowsheet.Options.SelectedComponents.Count = 0 Then
                MessageBox.Show(DWSIM.App.GetLocalString("Adicionesubstnciassi"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            ElseIf Me.CurrentFlowsheet.Options.PropertyPackages.Count = 0 Then
                MessageBox.Show(DWSIM.App.GetLocalString("NoexistemPacotesdePr"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            End If
        End If

        CurrentFlowsheet?.EnableUndoRedo()

    End Sub

    Sub Init(Optional ByVal reset As Boolean = False)

        'If Not SetHeights Then

        '    ogc1.RowTemplate.Height = 23 * Settings.DpiScale
        '    DataGridViewPP.RowTemplate.Height = 23 * Settings.DpiScale
        '    dgvpp.RowTemplate.Height = 23 * Settings.DpiScale

        '    ogc1.ColumnHeadersHeight *= Settings.DpiScale
        '    DataGridViewPP.ColumnHeadersHeight *= Settings.DpiScale
        '    dgvpp.ColumnHeadersHeight *= Settings.DpiScale

        '    SetHeights = True

        'End If

        CurrentFlowsheet.SetDirtyStatus()

        Dim pathsep As Char = Path.DirectorySeparatorChar

        Dim comp As BaseClasses.ConstantProperties

        If Not loaded Or reset Then

            ExtensionMethods.ChangeDefaultFont(Me)

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
                Dim data = New Object() {comp.Name, True, comp.Name, comp.Tag, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.CurrentDB, comp.IsCOOLPROPSupported}
                r.CreateCells(ogc1, data)
                r.Height = 23 * Settings.DpiScale
                rowlist.Add(r)
                CompoundList.Add(comp.CAS_Number)
                CompoundList.Add(comp.Formula)
                If Not Indexes.ContainsKey(comp.Name) Then Indexes.Add(comp.Name, ogc1.Rows.Count - 1)
                If Not Indexes.ContainsKey(comp.CAS_Number) Then Indexes.Add(comp.CAS_Number, ogc1.Rows.Count - 1)
                If Not Indexes.ContainsKey(comp.Formula) Then Indexes.Add(comp.Formula, ogc1.Rows.Count - 1)
            Next
            For Each comp In Me.CurrentFlowsheet.Options.NotSelectedComponents.Values
                Dim r As New DataGridViewRow()
                Dim data = New Object() {comp.Name, False, comp.Name, comp.Tag, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.CurrentDB, comp.IsCOOLPROPSupported}
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

            Dim addobj As Boolean = True

            'property packages
            Me.DataGridViewPP.Rows.Clear()
            For Each pp2 As PropertyPackages.PropertyPackage In FormMain.PropertyPackages.Values.OrderBy(Function(x) x.ComponentName)
                If Not CurrentFlowsheet.MobileCompatibilityMode Then
                    addobj = True
                Else
                    addobj = pp2.MobileCompatible
                End If
                If addobj Then Me.DataGridViewPP.Rows.Add(New Object() {pp2.ComponentName, pp2.GetDisplayIcon(), pp2.ComponentName, pp2.ComponentDescription})
            Next

            If Not FormMain.IsPro Then
                ProFeatures.Functions.AddProPPs2(DataGridViewPP)
            End If

            DataGridViewPP.Sort(DataGridViewPP.Columns(2), System.ComponentModel.ListSortDirection.Ascending)

            Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
            Dim unitopassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.UnitOperations")).FirstOrDefault

            aTypeList.Clear()
            aTypeList.AddRange(calculatorassembly.GetTypes().Where(Function(x)
                                                                       If x.GetInterface("DWSIM.Interfaces.ISimulationObject") IsNot Nothing Then
                                                                           Return True
                                                                       Else
                                                                           Return False
                                                                       End If
                                                                   End Function))
            aTypeList.AddRange(unitopassembly.GetTypes().Where(Function(x)
                                                                   If x.GetInterface("DWSIM.Interfaces.ISimulationObject") IsNot Nothing Then
                                                                       Return True
                                                                   Else
                                                                       Return False
                                                                   End If
                                                               End Function))

            Dim add As Boolean = False
            If CurrentFlowsheet.FlowsheetOptions.VisibleProperties.Count = 0 Then add = True

            cbObjectType.Items.Clear()
            availableproperties.Clear()
            aTypeRefs.Clear()
            For Each item In aTypeList.OrderBy(Function(x) x.Name)
                If Not item.IsAbstract Then
                    Dim obj = DirectCast(Activator.CreateInstance(item), Interfaces.ISimulationObject)
                    obj.SetFlowsheet(CurrentFlowsheet)
                    cbObjectType.Items.Add(obj.GetDisplayName)
                    availableproperties.Add(obj.GetDisplayName, obj.GetProperties(PropertyType.ALL))
                    aTypeRefs.Add(obj.GetDisplayName, item.Name)
                    If add Then CurrentFlowsheet.FlowsheetOptions.VisibleProperties.Add(item.Name, obj.GetDefaultProperties.ToList)
                    obj.SetFlowsheet(Nothing)
                    obj = Nothing
                End If
            Next
            cbObjectType.SelectedIndex = 0
            cbObjectType.Sorted = True

            With Me.dgvpp.Rows
                .Clear()
                For Each pp2 As PropertyPackages.PropertyPackage In CurrentFlowsheet.Options.PropertyPackages.Values
                    .Add(New Object() {pp2.UniqueID, pp2.Tag, pp2.ComponentName})
                Next
            End With

            Me.ComboBox2.Items.Clear()
            Me.ComboBox2.Items.AddRange(FormMain.AvailableUnitSystems.Keys.ToArray)

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

        ComboBox1.SelectedItem = Me.CurrentFlowsheet?.Options.NumberFormat
        ComboBox3.SelectedItem = Me.CurrentFlowsheet?.Options.FractionNumberFormat

        If Me.CurrentFlowsheet.Options.SelectedUnitSystem.Name <> "" Then
            ComboBox2.SelectedItem = Me.CurrentFlowsheet.Options.SelectedUnitSystem.Name
        Else
            ComboBox2.SelectedIndex = 0
        End If

        Me.TBaut.Text = Me.CurrentFlowsheet.Options.SimulationAuthor
        Me.TBdesc.Text = Me.CurrentFlowsheet.Options.SimulationComments.Replace(vbLf, vbCrLf)
        Me.TBtit.Text = Me.CurrentFlowsheet.Options.SimulationName

        Me.tbPassword.Text = CurrentFlowsheet.Options.Password
        Me.chkUsePassword.Checked = CurrentFlowsheet.Options.UsePassword

        If DWSIM.App.IsRunningOnMono Then btnConfigPP.Enabled = True

        cbMassBalanceCheck.SelectedIndex = CurrentFlowsheet.Options.MassBalanceCheck

        cbEnergyBalanceCheck.SelectedIndex = CurrentFlowsheet.Options.EnergyBalanceCheck

        tbMassBalTol.Text = CurrentFlowsheet.Options.MassBalanceRelativeTolerance.ToString()

        tbEnergyBalTol.Text = CurrentFlowsheet.Options.EnergyBalanceRelativeTolerance.ToString()

        chkShowFloatingTables.Checked = CurrentFlowsheet.Options.DisplayFloatingPropertyTables
        chkShowAnchoredPropertyLists.Checked = CurrentFlowsheet.Options.DisplayCornerPropertyList

        chkDisplayFloatingTableCompoundAmounts.Checked = CurrentFlowsheet.Options.DisplayFloatingTableCompoundAmounts
        cbDefaultFloatingTableCompoundAmountBasis.SelectedIndex = CurrentFlowsheet.Options.DefaultFloatingTableCompoundAmountBasis

        cbOrderCompoundsBy.SelectedIndex = CurrentFlowsheet.Options.CompoundOrderingMode

        chkSkipEqCalcs.Checked = CurrentFlowsheet.Options.SkipEquilibriumCalculationOnDefinedStreams

        chkShowExtraPropertiesEditor.Checked = CurrentFlowsheet.Options.DisplayUserDefinedPropertiesEditor

        cbSpecCalcMode.SelectedIndex = CurrentFlowsheet.Options.SpecCalculationMode

        chkForceObjectCalculation.Checked = CurrentFlowsheet.Options.ForceObjectSolving

        Select Case CurrentFlowsheet.Options.ForceStreamPhase
            Case ForcedPhase.None
                cbForcePhase.SelectedIndex = 0
            Case ForcedPhase.Vapor
                cbForcePhase.SelectedIndex = 1
            Case ForcedPhase.Liquid
                cbForcePhase.SelectedIndex = 2
            Case ForcedPhase.Solid
                cbForcePhase.SelectedIndex = 3
        End Select

        chkShowMSTemp.Checked = CurrentFlowsheet.Options.DisplayMaterialStreamTemperatureValue
        chkShowMSPressure.Checked = CurrentFlowsheet.Options.DisplayMaterialStreamPressureValue
        chkMSShowW.Checked = CurrentFlowsheet.Options.DisplayMaterialStreamMassFlowValue
        chkMSSHowM.Checked = CurrentFlowsheet.Options.DisplayMaterialStreamMolarFlowValue
        chkMSShowV.Checked = CurrentFlowsheet.Options.DisplayMaterialStreamVolFlowValue
        chkMSShowE.Checked = CurrentFlowsheet.Options.DisplayMaterialStreamEnergyFlowValue
        chkESShowE.Checked = CurrentFlowsheet.Options.DisplayEnergyStreamPowerValue

        chkShowDynProps.Checked = CurrentFlowsheet.Options.DisplayDynamicPropertyValues

        chkIncludeFlowsheetMessagesInFile.Checked = CurrentFlowsheet.Options.SaveFlowsheetMessagesInFile

        chkEnableUndoRedo.Checked = CurrentFlowsheet.Options.EnabledUndoRedo

        Me.loaded = True

        ExtensionMethods.ChangeDefaultFont(Me)

        AddHandler DataGridView1.EditingControlShowing, AddressOf Me.myDataGridView_EditingControlShowing

    End Sub

    Private Sub myDataGridView_EditingControlShowing(ByVal sender As Object, ByVal e As DataGridViewEditingControlShowingEventArgs)
        If (e.Control.GetType = GetType(DataGridViewComboBoxEditingControl)) Then
            Dim cmb As ComboBox = CType(e.Control, ComboBox)
            RemoveHandler DataGridView1.EditingControlShowing, AddressOf Me.cmb_SelectionChangeCommitted
            AddHandler cmb.SelectionChangeCommitted, AddressOf Me.cmb_SelectionChangeCommitted
        End If

    End Sub

    Private Sub cmb_SelectionChangeCommitted(ByVal sender As Object, ByVal e As EventArgs)
        DataGridView1.CurrentCell.Value = CType(sender, DataGridViewComboBoxEditingControl).EditingControlFormattedValue
    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBox1.SelectedIndexChanged
        CurrentFlowsheet.Options.NumberFormat = Me.ComboBox1.SelectedItem
        CurrentFlowsheet.UpdateOpenEditForms()
    End Sub

    Private Sub TBtit_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TBtit.TextChanged
        If Me.loaded Then
            Me.CurrentFlowsheet.Options.SimulationName = Me.TBtit.Text
            Me.CurrentFlowsheet.Text = Me.TBtit.Text + " (" + Me.CurrentFlowsheet.Options.FilePath + ")"
        End If
    End Sub

    Private Sub TBaut_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TBaut.TextChanged
        If Me.loaded Then Me.CurrentFlowsheet.Options.SimulationAuthor = Me.TBaut.Text
    End Sub

    Private Sub TBdesc_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TBdesc.TextChanged
        If Me.loaded Then Me.CurrentFlowsheet.Options.SimulationComments = Me.TBdesc.Text
    End Sub

    Private Sub Button2_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click

        Me.CurrentFlowsheet.FrmPCBulk.ShowDialog(Me)
    End Sub

    Public Sub ComboBox2_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBox2.SelectedIndexChanged

        CurrentFlowsheet.Options.SelectedUnitSystem = FormMain.AvailableUnitSystems.Item(ComboBox2.SelectedItem.ToString)
        Dim su As SystemsOfUnits.Units = CurrentFlowsheet.Options.SelectedUnitSystem

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

        CurrentFlowsheet.UpdateOpenEditForms()

    End Sub

    Private Sub DataGridView1_CellValueChanged1(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles DataGridView1.CellValueChanged

        If loaded And e.RowIndex >= 0 Then

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

            Me.CurrentFlowsheet.FormSurface.UpdateSelectedObject()

            CurrentFlowsheet.UpdateOpenEditForms()

        End If

    End Sub

    Private Sub KryptonButton15_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton15.Click
        Dim frmUnit As New FormUnitGen
        frmUnit.ShowDialog(Me)
    End Sub

    Private Sub KryptonButton18_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton18.Click

        If Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("SistemaSI") And
            Me.ComboBox2.SelectedItem <> "SI (Engineering)" And
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("SistemaCGS") And
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("SistemaIngls") And
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("Personalizado1BR") And
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("Personalizado2SC") And
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("Personalizado3CNTP") Then

            Dim str = Me.ComboBox2.SelectedItem

            CurrentFlowsheet.AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.SystemOfUnitsRemoved,
                  .NewValue = FormMain.AvailableUnitSystems(str),
                 .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_SystemOfUnitsRemoved"), FormMain.AvailableUnitSystems(str).Name)})

            My.Application.UserUnitSystems.Remove(str)

            FormMain.AvailableUnitSystems.Remove(Me.ComboBox2.SelectedItem)

            Me.ComboBox2.SelectedIndex = 0
            Me.ComboBox2.Items.Remove(str)

        Else

            MessageBox.Show(DWSIM.App.GetLocalString("EsteSistemadeUnidade"))

        End If


    End Sub

    Private Sub KryptonButton23_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton23.Click

        If Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("SistemaSI") And
            Me.ComboBox2.SelectedItem <> "SI (Engineering)" And
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("SistemaCGS") And
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("SistemaIngls") And
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("Personalizado1BR") And
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("Personalizado2SC") And
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("Personalizado3CNTP") Then

            Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

            Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("DWSIM System of Units File", "*.dwund")})

            If handler IsNot Nothing Then
                Using stream As New IO.MemoryStream()
                    Dim su As SystemsOfUnits.Units = CurrentFlowsheet.Options.SelectedUnitSystem
                    Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
                    Try
                        mySerializer.Serialize(stream, su)
                        handler.Write(stream)
                    Catch ex As System.Runtime.Serialization.SerializationException
                        MessageBox.Show(ex.Message, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End Try
                End Using
            End If

        Else
            MessageBox.Show(DWSIM.App.GetLocalString("EsteSistemadeUnidade"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
        End If

    End Sub

    Private Sub KryptonButton22_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton22.Click

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim openedFile As IVirtualFile = filePickerForm.ShowOpenDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("DWSIM System of Units File", "*.dwund")})

        If openedFile IsNot Nothing Then

            Using str = openedFile.OpenRead()
                Dim su As New SystemsOfUnits.Units
                Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
                Try
                    su = DirectCast(mySerializer.Deserialize(str), SystemsOfUnits.Units)
                    While FormMain.AvailableUnitSystems.ContainsKey(su.Name)
                        su.Name += "_1"
                    End While
                    FormMain.AvailableUnitSystems.Add(su.Name, su)
                    Me.ComboBox2.Items.Add(su.Name)
                    Me.CurrentFlowsheet.Options.SelectedUnitSystem.Name = su.Name
                    Dim array1(FormMain.AvailableUnitSystems.Count - 1) As String
                    FormMain.AvailableUnitSystems.Keys.CopyTo(array1, 0)
                    ComboBox2.SelectedItem = Me.CurrentFlowsheet.Options.SelectedUnitSystem.Name
                Catch ex As System.Runtime.Serialization.SerializationException
                    MessageBox.Show(ex.Message, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                End Try
            End Using

        End If

    End Sub

    Public Function AddCompToGrid(ByRef comp As BaseClasses.ConstantProperties) As Integer

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
                r.CreateCells(ogc1, New Object() {comp.Name, True, translatedname, comp.Tag, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
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

    Public Function GetCompRowIndex(ByRef comp As BaseClasses.ConstantProperties) As Integer

        For Each r As DataGridViewRow In ogc1.Rows

            If r.Cells(0).Value = comp.Name Then Return r.Index Else Return -1

        Next

    End Function

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
                'ogc1.Sort(colAdd, System.ComponentModel.ListSortDirection.Descending)
            Else
                For Each r As DataGridViewRow In ogc1.Rows
                    If Not r.Cells(2).Value Is Nothing Then
                        If r.Cells(2).Value.ToString.ToLower.Contains(txtSearch.Text.ToLower) Or
                           r.Cells(4).Value.ToString.ToLower.Contains(txtSearch.Text.ToLower) Or
                           r.Cells(6).Value.ToString.ToLower.Contains(txtSearch.Text.ToLower) Then
                            r.Visible = True
                            If r.Cells(2).Value.ToString.ToLower.Equals(txtSearch.Text.ToLower) Or
                                               r.Cells(4).Value.ToString.ToLower.Equals(txtSearch.Text.ToLower) Or
                                               r.Cells(6).Value.ToString.ToLower.Equals(txtSearch.Text.ToLower) Then
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

            'ogc1.FirstDisplayedScrollingRowIndex = 0
            'ogc1.Sort(colAdd, System.ComponentModel.ListSortDirection.Descending)

        End Try

        ogc1.ResumeLayout()

    End Sub

    Private Sub btnConfigPP_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnConfigPP.Click

        CurrentFlowsheet.RegisterSnapshot(SnapshotType.PropertyPackages)

        Dim ppid As String = ""
        If DWSIM.App.IsRunningOnMono Then
            ppid = dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value
        Else
            ppid = dgvpp.SelectedRows(0).Cells(0).Value
        End If
        Dim pp As PropertyPackages.PropertyPackage = CurrentFlowsheet.PropertyPackages(ppid)
        pp.DisplayGroupedEditingForm()

    End Sub

    Private Sub btnDeletePP_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnDeletePP.Click

        CurrentFlowsheet.RegisterSnapshot(SnapshotType.PropertyPackages)

        If DWSIM.App.IsRunningOnMono Then
            If dgvpp.SelectedCells.Count > 0 Then
                CurrentFlowsheet.AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.PropertyPackageRemoved,
                          .ObjID = dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value,
                          .NewValue = CurrentFlowsheet.Options.PropertyPackages(dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value).Clone,
                          .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_PropertyPackageRemoved"), dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(1).Value)})
                CurrentFlowsheet.Options.PropertyPackages.Remove(dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value)
                dgvpp.Rows.RemoveAt(dgvpp.SelectedCells(0).RowIndex)
            End If
        Else
            If Not dgvpp.SelectedRows.Count = 0 Then
                CurrentFlowsheet.AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.PropertyPackageRemoved,
                   .ObjID = dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value,
                   .NewValue = CurrentFlowsheet.Options.PropertyPackages(dgvpp.SelectedRows(0).Cells(0).Value).Clone,
                   .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_PropertyPackageRemoved"), dgvpp.SelectedRows(0).Cells(1).Value)})
                CurrentFlowsheet.Options.PropertyPackages.Remove(dgvpp.SelectedRows(0).Cells(0).Value)
                dgvpp.Rows.Remove(dgvpp.SelectedRows(0))
            End If
        End If
        If dgvpp.Rows.Count > 0 Then CurrentFlowsheet.UpdateOpenEditForms()

    End Sub

    Private Sub btnCopyPP_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCopyPP.Click

        Dim pp As PropertyPackages.PropertyPackage

        Try

            Dim ppid As String = ""
            If DWSIM.App.IsRunningOnMono Then
                ppid = dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value
            Else
                ppid = dgvpp.SelectedRows(0).Cells(0).Value
            End If
            pp = CurrentFlowsheet.Options.PropertyPackages(ppid).DeepClone
            pp.Flowsheet = CurrentFlowsheet
            With pp
                pp.Tag = pp.Tag & CStr(FormFlowsheet.Options.PropertyPackages.Count)
                pp.UniqueID = Guid.NewGuid.ToString
            End With
            CurrentFlowsheet.Options.PropertyPackages.Add(pp.UniqueID, pp)
            Me.dgvpp.Rows.Add(New Object() {pp.UniqueID, pp.Tag, pp.ComponentName})

            CurrentFlowsheet.UpdateOpenEditForms()

            CurrentFlowsheet.RegisterSnapshot(SnapshotType.PropertyPackages)

        Catch ex As Exception

        End Try

    End Sub

    Private Sub dgvpp_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvpp.CellValueChanged
        If loaded Then
            If e.RowIndex >= 0 Then CurrentFlowsheet.Options.PropertyPackages(dgvpp.Rows(e.RowIndex).Cells(0).Value).Tag = dgvpp.Rows(e.RowIndex).Cells(1).Value
        End If
    End Sub

    Private Sub dgvpp_SelectionChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles dgvpp.SelectionChanged
        If DWSIM.App.IsRunningOnMono Then
            If dgvpp.SelectedCells.Count > 0 Then
                If dgvpp.SelectedCells(0).RowIndex >= 0 Then
                    If dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value <> Nothing Then
                        btnDeletePP.Enabled = True
                        If CurrentFlowsheet.Options.PropertyPackages.ContainsKey(dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value) Then
                            If CurrentFlowsheet.Options.PropertyPackages(dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value).IsConfigurable Then
                                btnConfigPP.Enabled = True
                            Else
                                btnConfigPP.Enabled = False
                            End If
                            btnCopyPP.Enabled = True
                        End If
                    End If
                End If
            End If
        Else
            If dgvpp.SelectedRows.Count > 0 Then
                btnDeletePP.Enabled = True
                If CurrentFlowsheet.Options.PropertyPackages.ContainsKey(dgvpp.SelectedRows(0).Cells(0).Value) Then
                    If CurrentFlowsheet.Options.PropertyPackages(dgvpp.SelectedRows(0).Cells(0).Value).IsConfigurable Then
                        btnConfigPP.Enabled = True
                    Else
                        btnConfigPP.Enabled = False
                    End If
                    btnCopyPP.Enabled = True
                End If
            End If
        End If
    End Sub

    Private Sub Button9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button9.Click
        Dim frmdc As New DCCharacterizationWizard
        frmdc.ShowDialog(Me)
    End Sub

    Sub AddCompToSimulation(ByVal compid As String)

        If Me.loaded Then

            CurrentFlowsheet.RegisterSnapshot(SnapshotType.Compounds)

            If Not Me.CurrentFlowsheet.Options.SelectedComponents.ContainsKey(compid) Then

                Dim tmpcomp As New BaseClasses.ConstantProperties
                tmpcomp = Me.CurrentFlowsheet.Options.NotSelectedComponents(compid)

                Me.CurrentFlowsheet.Options.SelectedComponents.Add(tmpcomp.Name, tmpcomp)
                Me.CurrentFlowsheet.Options.NotSelectedComponents.Remove(tmpcomp.Name)

                For Each mstr In CurrentFlowsheet.Collections.FlowsheetObjectCollection.Values.Where(Function(x) TypeOf x Is Streams.MaterialStream)
                    For Each phase In mstr.Phases.Values
                        phase.Compounds.Add(tmpcomp.Name, New Compound(tmpcomp.Name, ""))
                        phase.Compounds(tmpcomp.Name).ConstantProperties = tmpcomp
                    Next
                Next

                CurrentFlowsheet.UpdateOpenEditForms()

            End If


        End If

    End Sub

    Sub RemoveCompFromSimulation(ByVal compid As String)

        CurrentFlowsheet.RegisterSnapshot(SnapshotType.Compounds)

        Dim tmpcomp As New BaseClasses.ConstantProperties
        Dim nm As String = compid
        tmpcomp = Me.CurrentFlowsheet.Options.SelectedComponents(nm)
        Me.CurrentFlowsheet.Options.SelectedComponents.Remove(tmpcomp.Name)
        Me.CurrentFlowsheet.Options.NotSelectedComponents.Add(tmpcomp.Name, tmpcomp)
        Me.AddCompToGrid(tmpcomp)
        Dim ms As Streams.MaterialStream
        Dim proplist As New ArrayList

        For Each ms In CurrentFlowsheet.Collections.FlowsheetObjectCollection.Values.Where(Function(x) TypeOf x Is Streams.MaterialStream)
            Dim amount As Double = 0#
            If ms.Phases(0).Properties.massflow.HasValue Then
                amount = ms.Phases(0).Compounds(tmpcomp.Name).MassFlow.GetValueOrDefault
                ms.Phases(0).Properties.massflow -= amount
            End If
            If ms.Phases(0).Properties.molarflow.HasValue Then
                amount = ms.Phases(0).Compounds(tmpcomp.Name).MolarFlow.GetValueOrDefault
                ms.Phases(0).Properties.molarflow -= amount
            End If
            If ms.Phases(0).Properties.volumetric_flow.HasValue Then
                amount = ms.Phases(0).Compounds(tmpcomp.Name).VolumetricFlow.GetValueOrDefault
                ms.Phases(0).Properties.volumetric_flow -= amount
            End If
            For Each phase In ms.Phases.Values
                phase.Compounds.Remove(tmpcomp.Name)
            Next
            ms.ClearCalculatedProps()
            ms.NormalizeOverallMoleComposition()
            ms.NormalizeOverallMassComposition()
            ms.Calculated = False
            ms.GraphicObject.Calculated = False
        Next

        For Each pp In CurrentFlowsheet.Options.PropertyPackages.Values
            If DirectCast(pp, PropertyPackage).ForcedSolids.Contains(compid) Then
                DirectCast(pp, PropertyPackage).ForcedSolids.Remove(compid)
            End If
        Next

        CurrentFlowsheet.UpdateOpenEditForms()

    End Sub

    Private Sub ogc1_DataError(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles ogc1.DataError

    End Sub

    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click

        CurrentFlowsheet.RegisterSnapshot(SnapshotType.PropertyPackages)

        If DataGridViewPP.SelectedRows(0).Cells(0).Value = "" Then
            ProFeatures.Functions.DisplayTransitionForm(CurrentFlowsheet, DataGridViewPP.SelectedRows(0).Cells(2).Value + " Property Package")
            Exit Sub
        End If

        Dim pp As PropertyPackages.PropertyPackage
        pp = FormMain.PropertyPackages(Me.DataGridViewPP.SelectedRows(0).Cells(0).Value).Clone()

        If pp Is Nothing Then Exit Sub

        With pp
            pp.Tag = pp.ComponentName + " (" + (CurrentFlowsheet.PropertyPackages.Count + 1).ToString() + ")"
            pp.UniqueID = "PP-" & Guid.NewGuid.ToString
            pp.Flowsheet = CurrentFlowsheet
        End With

        CurrentFlowsheet.Options.PropertyPackages.Add(pp.UniqueID, pp)
        Me.dgvpp.Rows.Add(New Object() {pp.UniqueID, pp.Tag, pp.ComponentName})

        FormMain.AnalyticsProvider?.RegisterEvent("Property Package Added", pp.ComponentName, Nothing)

        CurrentFlowsheet.UpdateOpenEditForms()

    End Sub

    Private Sub ListViewPP_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DataGridViewPP.SelectionChanged
        If Me.DataGridViewPP.SelectedRows.Count > 0 Then
            Me.Button8.Enabled = True
        Else
            Me.Button8.Enabled = False
        End If
    End Sub

    Private Sub ComboBox3_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBox3.SelectedIndexChanged
        CurrentFlowsheet.Options.FractionNumberFormat = Me.ComboBox3.SelectedItem
        CurrentFlowsheet.UpdateOpenEditForms()
    End Sub

    Private Sub TextBox1_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtSearch.KeyDown
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

    Private Sub chkUsePassword_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles chkUsePassword.CheckedChanged
        If chkUsePassword.Checked Then tbPassword.Enabled = True Else tbPassword.Enabled = False
        CurrentFlowsheet.Options.UsePassword = chkUsePassword.Checked
    End Sub

    Private Sub tbPassword_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tbPassword.TextChanged
        CurrentFlowsheet.Options.Password = tbPassword.Text
    End Sub

    Private Sub DataGridView1_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles DataGridView1.DataError

    End Sub

    Private Sub Button3_Click_2(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click

        Dim frmam As New FormAssayManager
        frmam.ShowDialog(Me)

    End Sub

    Private Sub LinkLabelPropertyMethods_LinkClicked(sender As System.Object, e As System.Windows.Forms.LinkLabelLinkClickedEventArgs)
        Process.Start("https://dwsim.org/wiki/index.php?title=Property_Methods_and_Correlation_Profiles")
    End Sub

    Private Sub ogc1_CellDoubleClick(sender As Object, e As DataGridViewCellEventArgs) Handles ogc1.CellDoubleClick
        If e.RowIndex > -1 Then
            ogc1.Rows(e.RowIndex).Cells(1).Value = Not ogc1.Rows(e.RowIndex).Cells(1).Value
        End If
    End Sub

    Private Sub ListViewPP_DoubleClick(sender As Object, e As EventArgs) Handles DataGridViewPP.DoubleClick
        If Me.DataGridViewPP.SelectedRows.Count = 1 Then
            Button8.PerformClick()
        End If
    End Sub

    Private Sub Button12_Click(sender As Object, e As EventArgs)
        txtSearch.Text = ""
    End Sub

    Private Sub cbObjectType_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbObjectType.SelectedIndexChanged
        If Not loaded Then Exit Sub
        PropertyListView.Items.Clear()
        For Each item In availableproperties(cbObjectType.SelectedItem)
            PropertyListView.Items.Add(item, CurrentFlowsheet.GetTranslatedString1(item), 0).Tag = item
        Next

        For Each item In availableproperties(cbObjectType.SelectedItem)
            If CurrentFlowsheet.FlowsheetOptions.VisibleProperties(aTypeRefs(cbObjectType.SelectedItem)).Contains(item) Then
                PropertyListView.Items(item).Checked = True
            End If
        Next

    End Sub

    Private Sub PropertyListView_ItemChecked(sender As Object, e As ItemCheckedEventArgs) Handles PropertyListView.ItemChecked
        If loaded Then
            If e.Item.Checked Then
                If Not CurrentFlowsheet.FlowsheetOptions.VisibleProperties(aTypeRefs(cbObjectType.SelectedItem)).Contains(e.Item.Tag) Then
                    CurrentFlowsheet.FlowsheetOptions.VisibleProperties(aTypeRefs(cbObjectType.SelectedItem)).Add(e.Item.Tag)
                End If
            Else
                If CurrentFlowsheet.FlowsheetOptions.VisibleProperties(aTypeRefs(cbObjectType.SelectedItem)).Contains(e.Item.Tag) Then
                    CurrentFlowsheet.FlowsheetOptions.VisibleProperties(aTypeRefs(cbObjectType.SelectedItem)).Remove(e.Item.Tag)
                End If
            End If
        End If
    End Sub

    Private Sub tsbClose_Click(sender As Object, e As EventArgs)
        If DWSIM.App.IsRunningOnMono Then
            Me.Close()
        Else
            Me.Hide()
        End If
    End Sub

    Private Sub btnInfoLeft_Click(sender As Object, e As EventArgs) Handles btnInfoLeft.Click
        FormMain.AnalyticsProvider?.RegisterEvent("Viewing Selected Compound", "", Nothing)
        Dim compound As Interfaces.ICompoundConstantProperties
        Dim compID As String = ""
        If DWSIM.App.IsRunningOnMono Then
            compID = ogc1.Rows(ogc1.SelectedCells(0).RowIndex).Cells(0).Value
        Else
            compID = ogc1.SelectedRows(0).Cells(0).Value
        End If
        If CurrentFlowsheet.AvailableCompounds.ContainsKey(compID) Then
            compound = Me.CurrentFlowsheet.AvailableCompounds(compID)
        ElseIf CurrentFlowsheet.Options.SelectedComponents.ContainsKey(compID) Then
            compound = Me.CurrentFlowsheet.Options.SelectedComponents(compID)
        ElseIf CurrentFlowsheet.Options.NotSelectedComponents.ContainsKey(compID) Then
            compound = Me.CurrentFlowsheet.Options.NotSelectedComponents(compID)
        Else
            compound = Nothing
        End If
        Dim f As New FormPureComp() With {.Flowsheet = CurrentFlowsheet, .Added = False, .MyCompound = compound}
        CurrentFlowsheet.DisplayForm(f)
    End Sub

    Private Sub btnSelectAll_Click(sender As Object, e As EventArgs) Handles btnSelectAll.Click
        For Each item As ListViewItem In PropertyListView.Items
            item.Checked = True
        Next
    End Sub

    Private Sub btnClearSelection_Click(sender As Object, e As EventArgs) Handles btnClearSelection.Click
        For Each item As ListViewItem In PropertyListView.Items
            item.Checked = False
        Next
    End Sub

    Private Sub Button1_Click_1(sender As Object, e As EventArgs) Handles Button1.Click

        FormMain.AnalyticsProvider?.RegisterEvent("Importing Compounds from JSON Files", "", Nothing)

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim openedFile As IVirtualFile = filePickerForm.ShowOpenDialog(New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON file", "*.json")})
        If openedFile IsNot Nothing Then
            Try
                Dim comp = Newtonsoft.Json.JsonConvert.DeserializeObject(Of BaseClasses.ConstantProperties)(openedFile.ReadAllText())
                If Not Me.CurrentFlowsheet.AvailableCompounds.ContainsKey(comp.Name) Then
                    Me.CurrentFlowsheet.AvailableCompounds.Add(comp.Name, comp)
                    Me.CurrentFlowsheet.Options.SelectedComponents.Add(comp.Name, comp)
                    Me.CurrentFlowsheet.Options.NotSelectedComponents.Remove(comp.Name)
                    Dim ms As Streams.MaterialStream
                    Dim proplist As New ArrayList
                    For Each ms In CurrentFlowsheet.Collections.FlowsheetObjectCollection.Values.Where(Function(x) TypeOf x Is Streams.MaterialStream)
                        For Each phase As BaseClasses.Phase In ms.Phases.Values
                            phase.Compounds.Add(comp.Name, New BaseClasses.Compound(comp.Name, ""))
                            phase.Compounds(comp.Name).ConstantProperties = comp
                        Next
                    Next
                    ogc1.Rows.Add(New Object() {comp.Name, True, comp.Name, comp.Tag, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
                    ogc1.ClearSelection()
                    ogc1.Sort(colAdd, System.ComponentModel.ListSortDirection.Descending)
                Else
                    'compound exists.
                    If MessageBox.Show(DWSIM.App.GetLocalString("UpdateFromJSON"), "DWSIM", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = DialogResult.Yes Then
                        If CurrentFlowsheet.AvailableCompounds.ContainsKey(comp.Name) Then
                            'compound exists but not selected (added)
                            Dim c0 = Me.CurrentFlowsheet.AvailableCompounds(comp.Name)
                            DirectCast(c0, ICustomXMLSerialization).LoadData(DirectCast(comp, ICustomXMLSerialization).SaveData())
                            For i = 0 To ogc1.Rows.Count - 1
                                If ogc1.Rows(i).Cells(0).Value = comp.Name Then
                                    ogc1.Rows(i).Cells(1).Value = True
                                    Exit For
                                End If
                            Next
                            ogc1.Sort(colAdd, System.ComponentModel.ListSortDirection.Descending)
                        Else
                            'compound exists + added
                            Dim c0 = Me.CurrentFlowsheet.Options.SelectedComponents(comp.Name)
                            DirectCast(c0, ICustomXMLSerialization).LoadData(DirectCast(comp, ICustomXMLSerialization).SaveData())
                            Dim ms As Streams.MaterialStream
                            Dim proplist As New ArrayList
                            For Each ms In CurrentFlowsheet.Collections.FlowsheetObjectCollection.Values.Where(Function(x) TypeOf x Is Streams.MaterialStream)
                                For Each phase As BaseClasses.Phase In ms.Phases.Values
                                    phase.Compounds(comp.Name).ConstantProperties = c0
                                Next
                            Next
                        End If
                        MessageBox.Show(DWSIM.App.GetLocalString("CompoundUpdated"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Information)
                    End If
                End If
                CurrentFlowsheet.UpdateOpenEditForms()
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ": " + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If

    End Sub

    Private Sub Button13_Click(sender As Object, e As EventArgs) Handles Button13.Click

        FormMain.AnalyticsProvider?.RegisterEvent("Importing Compounds from Online Sources", "", Nothing)

        Dim f As New FormImportCompoundOnline
        If f.ShowDialog(Me) = System.Windows.Forms.DialogResult.OK Then
            Try
                Dim comp = f.BaseCompound
                If Not Me.CurrentFlowsheet.AvailableCompounds.ContainsKey(comp.Name) Then
                    Me.CurrentFlowsheet.AvailableCompounds.Add(comp.Name, comp)
                    Me.CurrentFlowsheet.Options.SelectedComponents.Add(comp.Name, comp)
                    Me.CurrentFlowsheet.Options.NotSelectedComponents.Remove(comp.Name)
                    Dim ms As Streams.MaterialStream
                    Dim proplist As New ArrayList
                    For Each ms In CurrentFlowsheet.Collections.FlowsheetObjectCollection.Values.Where(Function(x) TypeOf x Is Streams.MaterialStream)
                        For Each phase As BaseClasses.Phase In ms.Phases.Values
                            phase.Compounds.Add(comp.Name, New BaseClasses.Compound(comp.Name, ""))
                            phase.Compounds(comp.Name).ConstantProperties = comp
                        Next
                    Next
                    ogc1.Rows.Add(New Object() {comp.Name, True, comp.Name, comp.Tag, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
                    ogc1.ClearSelection()
                    ogc1.Sort(colAdd, System.ComponentModel.ListSortDirection.Descending)
                Else
                    MessageBox.Show(DWSIM.App.GetLocalString("CompoundExists"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End If
                CurrentFlowsheet.UpdateOpenEditForms()
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If
    End Sub

    Private Sub cbMassBalanceCheck_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbMassBalanceCheck.SelectedIndexChanged
        CurrentFlowsheet.Options.MassBalanceCheck = cbMassBalanceCheck.SelectedIndex
    End Sub

    Private Sub cbEnergyBalanceCheck_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbEnergyBalanceCheck.SelectedIndexChanged
        CurrentFlowsheet.Options.EnergyBalanceCheck = cbEnergyBalanceCheck.SelectedIndex
    End Sub

    Private Sub tbMassBalTol_TextChanged(sender As Object, e As EventArgs) Handles tbMassBalTol.TextChanged
        If tbMassBalTol.Text.IsValidDouble Then CurrentFlowsheet.Options.MassBalanceRelativeTolerance = tbMassBalTol.Text.ToDoubleFromCurrent
    End Sub

    Private Sub tbEnergyBalTol_TextChanged(sender As Object, e As EventArgs) Handles tbEnergyBalTol.TextChanged
        If tbEnergyBalTol.Text.IsValidDouble Then CurrentFlowsheet.Options.EnergyBalanceRelativeTolerance = tbEnergyBalTol.Text.ToDoubleFromCurrent
    End Sub

    Private Sub chkShowFloatingTables_CheckedChanged(sender As Object, e As EventArgs) Handles chkShowFloatingTables.CheckedChanged
        CurrentFlowsheet.Options.DisplayFloatingPropertyTables = chkShowFloatingTables.Checked
        CurrentFlowsheet.FormSurface.FlowsheetSurface.DrawFloatingTable = chkShowFloatingTables.Checked
    End Sub

    Private Sub chkShowAnchoredPropertyLists_CheckedChanged(sender As Object, e As EventArgs) Handles chkShowAnchoredPropertyLists.CheckedChanged
        CurrentFlowsheet.Options.DisplayCornerPropertyList = chkShowAnchoredPropertyLists.Checked
        CurrentFlowsheet.FormSurface.FlowsheetSurface.DrawPropertyList = chkShowAnchoredPropertyLists.Checked
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click

        FontDialog1.Font = New Font(CurrentFlowsheet.Options.DisplayCornerPropertyListFontName, CurrentFlowsheet.Options.DisplayCornerPropertyListFontSize, System.Drawing.FontStyle.Bold)

        Dim colortype = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("System.Drawing,")).FirstOrDefault().GetType("System.Drawing.KnownColor")
        Dim mycolor = [Enum].Parse(colortype, CurrentFlowsheet.Options.DisplayCornerPropertyListFontColor, True)

        FontDialog1.Color = Color.FromKnownColor(mycolor)
        FontDialog1.ShowColor = True
        FontDialog1.ShowDialog(Me)

        CurrentFlowsheet.Options.DisplayCornerPropertyListFontName = FontDialog1.Font.FontFamily.Name
        CurrentFlowsheet.Options.DisplayCornerPropertyListFontColor = FontDialog1.Color.Name
        CurrentFlowsheet.Options.DisplayCornerPropertyListFontSize = FontDialog1.Font.Size

    End Sub

    Private Sub chkDisplayFloatingTableCompoundAmounts_CheckedChanged(sender As Object, e As EventArgs) Handles chkDisplayFloatingTableCompoundAmounts.CheckedChanged
        CurrentFlowsheet.Options.DisplayFloatingTableCompoundAmounts = chkDisplayFloatingTableCompoundAmounts.Checked
    End Sub

    Private Sub cbDefaultFloatingTableCompoundAmountBasis_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbDefaultFloatingTableCompoundAmountBasis.SelectedIndexChanged
        CurrentFlowsheet.Options.DefaultFloatingTableCompoundAmountBasis = cbDefaultFloatingTableCompoundAmountBasis.SelectedIndex
    End Sub

    Private Sub btnConfigPPAdv_Click(sender As Object, e As EventArgs)
        Dim ppid As String = ""
        If GlobalSettings.Settings.IsRunningOnMono Then
            ppid = dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value
        Else
            ppid = dgvpp.SelectedRows(0).Cells(0).Value
        End If
        Dim pp As PropertyPackages.PropertyPackage = CurrentFlowsheet.Options.PropertyPackages(ppid)

        pp.DisplayAdvancedEditingForm()

        'start dispatcher for WPF Interop
        If Not GlobalSettings.Settings.IsRunningOnMono Then
            System.Windows.Threading.Dispatcher.Run()
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

            ElseIf ((e.ColumnIndex = colTag.Index) AndAlso (e.RowIndex <> -1)) Then

                Dim cid = ogc1.Rows(e.RowIndex).Cells(0).Value.ToString

                If CurrentFlowsheet.Options.SelectedComponents.ContainsKey(cid) Then
                    CurrentFlowsheet.Options.SelectedComponents(cid).Tag = ogc1.Rows(e.RowIndex).Cells(colTag.Index).Value
                Else
                    CurrentFlowsheet.Options.NotSelectedComponents(cid).Tag = ogc1.Rows(e.RowIndex).Cells(colTag.Index).Value
                End If

            End If

        End If

    End Sub

    Private Sub ogc1_OnCellMouseUp(ByVal sender As Object, ByVal e As DataGridViewCellMouseEventArgs) Handles ogc1.CellMouseUp

        If ((e.ColumnIndex = colAdd.Index) AndAlso (e.RowIndex <> -1)) Then
            ogc1.EndEdit()
        End If

    End Sub

    Private Sub cbOrderCompoundsBy_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbOrderCompoundsBy.SelectedIndexChanged
        CurrentFlowsheet.Options.CompoundOrderingMode = cbOrderCompoundsBy.SelectedIndex
        CurrentFlowsheet.UpdateOpenEditForms()
    End Sub

    Private Sub BtnCloneSI_Click(sender As Object, e As EventArgs) Handles btnCloneSI.Click

        Dim newsu = Newtonsoft.Json.JsonConvert.DeserializeObject(Of SystemsOfUnits.Units)(Newtonsoft.Json.JsonConvert.SerializeObject(CurrentFlowsheet.Options.SelectedUnitSystem))

        Dim cnt As Integer = 1
        While FormMain.AvailableUnitSystems.ContainsKey(newsu.Name)
            newsu.Name = CurrentFlowsheet.Options.SelectedUnitSystem.Name & "_" & cnt.ToString
            cnt += 1
        End While

        CurrentFlowsheet.AddUnitSystem(newsu)
        CurrentFlowsheet.AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.SystemOfUnitsAdded,
                                             .NewValue = newsu,
                                             .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_SystemOfUnitsAdded"), newsu.Name)})

        ComboBox2.SelectedIndex = ComboBox2.Items.Count - 1

    End Sub

    Private Sub chkSkipEqCalcs_CheckedChanged(sender As Object, e As EventArgs) Handles chkSkipEqCalcs.CheckedChanged
        CurrentFlowsheet.Options.SkipEquilibriumCalculationOnDefinedStreams = chkSkipEqCalcs.Checked
    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs)

        Dim ppid As String = ""
        If DWSIM.App.IsRunningOnMono Then
            ppid = dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value
        Else
            ppid = dgvpp.SelectedRows(0).Cells(0).Value
        End If
        Dim pp As PropertyPackage = CurrentFlowsheet.PropertyPackages(ppid)
        pp.DisplayFlashConfigForm()
    End Sub

    Private Sub Button5_Click_1(sender As Object, e As EventArgs) Handles Button5.Click
        txtSearch.Text = ""
    End Sub

    Private Sub LinkLabel1_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel1.LinkClicked
        Process.Start("https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-numeric-format-strings")
    End Sub

    Private Sub cbForcePhase_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbForcePhase.SelectedIndexChanged
        If loaded Then
            Select Case cbForcePhase.SelectedIndex
                Case 0
                    CurrentFlowsheet.Options.ForceStreamPhase = ForcedPhase.None
                Case 1
                    CurrentFlowsheet.Options.ForceStreamPhase = ForcedPhase.Vapor
                Case 2
                    CurrentFlowsheet.Options.ForceStreamPhase = ForcedPhase.Liquid
                Case 3
                    CurrentFlowsheet.Options.ForceStreamPhase = ForcedPhase.Solid
            End Select
        End If
    End Sub

    Private Sub chkShowExtraPropertiesEditor_CheckedChanged(sender As Object, e As EventArgs) Handles chkShowExtraPropertiesEditor.CheckedChanged
        CurrentFlowsheet.Options.DisplayUserDefinedPropertiesEditor = chkShowExtraPropertiesEditor.Checked
    End Sub

    Private Sub chkShowMSTemp_CheckedChanged(sender As Object, e As EventArgs) Handles chkShowMSTemp.CheckedChanged
        CurrentFlowsheet.Options.DisplayMaterialStreamTemperatureValue = chkShowMSTemp.Checked
    End Sub

    Private Sub chkShowMSPressure_CheckedChanged(sender As Object, e As EventArgs) Handles chkShowMSPressure.CheckedChanged
        CurrentFlowsheet.Options.DisplayMaterialStreamPressureValue = chkShowMSPressure.Checked
    End Sub

    Private Sub chkMSShowW_CheckedChanged(sender As Object, e As EventArgs) Handles chkMSShowW.CheckedChanged
        CurrentFlowsheet.Options.DisplayMaterialStreamMassFlowValue = chkMSShowW.Checked
    End Sub

    Private Sub chkMSSHowM_CheckedChanged(sender As Object, e As EventArgs) Handles chkMSSHowM.CheckedChanged
        CurrentFlowsheet.Options.DisplayMaterialStreamMolarFlowValue = chkMSSHowM.Checked
    End Sub

    Private Sub chkMSShowV_CheckedChanged(sender As Object, e As EventArgs) Handles chkMSShowV.CheckedChanged
        CurrentFlowsheet.Options.DisplayMaterialStreamVolFlowValue = chkMSShowV.Checked
    End Sub

    Private Sub chkMSShowE_CheckedChanged(sender As Object, e As EventArgs) Handles chkMSShowE.CheckedChanged
        CurrentFlowsheet.Options.DisplayMaterialStreamEnergyFlowValue = chkMSShowE.Checked
    End Sub

    Private Sub chkESShowE_CheckedChanged(sender As Object, e As EventArgs) Handles chkESShowE.CheckedChanged
        CurrentFlowsheet.Options.DisplayEnergyStreamPowerValue = chkESShowE.Checked
    End Sub

    Private Sub chkShowDynProps_CheckedChanged(sender As Object, e As EventArgs) Handles chkShowDynProps.CheckedChanged
        CurrentFlowsheet.Options.DisplayDynamicPropertyValues = chkShowDynProps.Checked
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click

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
                    CurrentFlowsheet.UpdateOpenEditForms()
                Else
                    MessageBox.Show(DWSIM.App.GetLocalString("CompoundExists"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End If
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If
    End Sub

    Private Sub cbSpecCalcMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSpecCalcMode.SelectedIndexChanged

        CurrentFlowsheet.Options.SpecCalculationMode = cbSpecCalcMode.SelectedIndex

    End Sub

    Private Sub chkForceObjectCalculation_CheckedChanged(sender As Object, e As EventArgs) Handles chkForceObjectCalculation.CheckedChanged

        CurrentFlowsheet.Options.ForceObjectSolving = chkForceObjectCalculation.Checked
        FormMain.AnalyticsProvider?.RegisterEvent("Smart Object Solver Enabled", Not CurrentFlowsheet.Options.ForceObjectSolving, Nothing)

    End Sub

    Private Sub chkIncludeFlowsheetMessagesInFile_CheckedChanged(sender As Object, e As EventArgs) Handles chkIncludeFlowsheetMessagesInFile.CheckedChanged

        CurrentFlowsheet.Options.SaveFlowsheetMessagesInFile = chkIncludeFlowsheetMessagesInFile.Checked

    End Sub

    Private Sub tsmiViewComp_Click(sender As Object, e As EventArgs) Handles tsmiViewComp.Click
        btnInfoLeft_Click(sender, e)
    End Sub

    Private Sub tsmiExportJSON_Click(sender As Object, e As EventArgs) Handles tsmiExportJSON.Click

        FormMain.AnalyticsProvider?.RegisterEvent("Exporting Compound to JSON", "", Nothing)

        Dim compound As Interfaces.ICompoundConstantProperties
        Dim compID As String
        If DWSIM.App.IsRunningOnMono Then
            compID = ogc1.Rows(ogc1.SelectedCells(0).RowIndex).Cells(0).Value
        Else
            compID = ogc1.SelectedRows(0).Cells(0).Value
        End If
        If CurrentFlowsheet.AvailableCompounds.ContainsKey(compID) Then
            compound = Me.CurrentFlowsheet.AvailableCompounds(compID)
        ElseIf CurrentFlowsheet.Options.SelectedComponents.ContainsKey(compID) Then
            compound = Me.CurrentFlowsheet.Options.SelectedComponents(compID)
        ElseIf CurrentFlowsheet.Options.NotSelectedComponents.ContainsKey(compID) Then
            compound = Me.CurrentFlowsheet.Options.NotSelectedComponents(compID)
        Else
            compound = Nothing
        End If

        If compound IsNot Nothing Then

            Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON File", "*.json")})

        If handler IsNot Nothing Then
            Using stream As New IO.MemoryStream()
                Using writer As New StreamWriter(stream) With {.AutoFlush = True}
                    Try
                            Dim jsondata = Newtonsoft.Json.JsonConvert.SerializeObject(compound, Newtonsoft.Json.Formatting.Indented)
                            writer.Write(jsondata)
                        handler.Write(stream)
                        MessageBox.Show(DWSIM.App.GetLocalString("FileSaved"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Information)
                    Catch ex As Exception
                        MessageBox.Show(DWSIM.App.GetLocalString("Erroaosalvararquivo") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End Try
                End Using
            End Using
        End If

        End If

    End Sub

    Private Sub tsmiReplace_Click(sender As Object, e As EventArgs) Handles tsmiReplace.Click


        Dim compound As Interfaces.ICompoundConstantProperties
        Dim compID As String
        If DWSIM.App.IsRunningOnMono Then
            compID = ogc1.Rows(ogc1.SelectedCells(0).RowIndex).Cells(0).Value
        Else
            compID = ogc1.SelectedRows(0).Cells(0).Value
        End If
        If CurrentFlowsheet.Options.SelectedComponents.ContainsKey(compID) Then
            compound = Me.CurrentFlowsheet.Options.SelectedComponents(compID)
        Else
            compound = Nothing
        End If

        If compound IsNot Nothing Then

            Dim fsc As New FormCompoundList With {.Compounds = CurrentFlowsheet.AvailableCompounds.Keys.ToList()}
            fsc.ShowDialog()

            If fsc.SelectedCompound IsNot Nothing Then

                Dim replacewith = Me.CurrentFlowsheet.AvailableCompounds(fsc.SelectedCompound)

                Dim oldc = compound.Name
                Dim newc = replacewith.Name

                AddCompToSimulation(newc)

                For Each mstr As Streams.MaterialStream In CurrentFlowsheet.Collections.FlowsheetObjectCollection.Values.Where(Function(x) TypeOf x Is Streams.MaterialStream)
                    For Each phase In mstr.Phases.Values
                        phase.Compounds(newc).MoleFraction = phase.Compounds(oldc).MoleFraction
                        phase.Compounds(newc).MassFraction = phase.Compounds(oldc).MassFraction
                    Next
                Next

                RemoveCompFromSimulation(oldc)

                For Each obj In CurrentFlowsheet.SimulationObjects.Values
                    obj.Calculated = False
                    obj.GraphicObject.Calculated = False
                Next

                CurrentFlowsheet.UpdateOpenEditForms()

                Init(True)

            End If

        End If


    End Sub

    Private Sub FormSimulSettings_Shown(sender As Object, e As EventArgs) Handles Me.Shown

        For Each r As DataGridViewRow In ogc1.Rows
            r.Selected = False
            r.Visible = True
        Next
        ogc1.Sort(colAdd, System.ComponentModel.ListSortDirection.Descending)
        FormMain.TranslateFormFunction?.Invoke(Me)

    End Sub

    Private Sub chkEnableUndoRedo_CheckedChanged(sender As Object, e As EventArgs) Handles chkEnableUndoRedo.CheckedChanged

        CurrentFlowsheet.Options.EnabledUndoRedo = chkEnableUndoRedo.Checked

        FormMain.AnalyticsProvider?.RegisterEvent("Undo/Redo Enabled/Disabled", CurrentFlowsheet.Options.EnabledUndoRedo, Nothing)

    End Sub

    Private Sub FormSimulSettings_VisibleChanged(sender As Object, e As EventArgs) Handles Me.VisibleChanged

        If Visible Then
            CurrentFlowsheet.RegisterSnapshot(SnapshotType.SimulationSettings)
        End If

    End Sub

End Class
