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

Public Class FormSimulSettings

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Private FrmChild As FormFlowsheet
    Public loaded As Boolean = False
    Public initialized As Boolean = False
    Public supports As Boolean = True

    Private availableproperties As New Dictionary(Of String, String())
    Private aTypeList As New List(Of Type)
    Private aTypeRefs As New Dictionary(Of String, String)

    Private prevsort As System.ComponentModel.ListSortDirection = System.ComponentModel.ListSortDirection.Ascending
    Private prevcol As Integer = 1

    Dim vdPP, vdSR As MessageBox()

    Private Sub FormSimulSettings_DockStateChanged(sender As Object, e As EventArgs) Handles Me.DockStateChanged
        If Not Me.DockHandler Is Nothing OrElse Not Me.DockHandler.FloatPane Is Nothing Then
            ' set the bounds of this form's FloatWindow to our desired position and size
            If Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Float Then
                Dim floatWin = Me.DockHandler.FloatPane.FloatWindow
                If Not floatWin Is Nothing Then
                    floatWin.SetBounds(floatWin.Location.X, floatWin.Location.Y, 900, 600)
                End If
            End If

            Try
                Init()
            Catch ex As Exception

            End Try

        End If
    End Sub

    Private Sub FormStSim_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Me.TabText = Me.Text

        initialized = True

        If DWSIM.App.IsRunningOnMono Then
            Me.ogc1.SelectionMode = DataGridViewSelectionMode.CellSelect
            Me.dgvpp.SelectionMode = DataGridViewSelectionMode.CellSelect
        End If

        Dim rm As New FormReacManager
        rm.Dock = DockStyle.Fill
        TabPageReactions.Controls.Add(rm)

        Init()

    End Sub

    Private Sub FormStSim_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing

        If Me.FrmChild.Options.SelectedComponents.Count = 0 And Me.FrmChild.Options.PropertyPackages.Count = 0 Then
            MessageBox.Show(DWSIM.App.GetLocalString("Adicionesubstnciassi"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            MessageBox.Show(DWSIM.App.GetLocalString("NoexistemPacotesdePr"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            'e.Cancel = True
        ElseIf Me.FrmChild.Options.SelectedComponents.Count = 0 Then
            MessageBox.Show(DWSIM.App.GetLocalString("Adicionesubstnciassi"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            'e.Cancel = True
        ElseIf Me.FrmChild.Options.PropertyPackages.Count = 0 Then
            MessageBox.Show(DWSIM.App.GetLocalString("NoexistemPacotesdePr"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            'e.Cancel = True
        Else

        End If

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

            For Each comp In Me.FrmChild.Options.SelectedComponents.Values
                ogc1.Rows.Add(New Object() {comp.Name, True, comp.Name, comp.Tag, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
            Next
            For Each comp In Me.FrmChild.Options.NotSelectedComponents.Values
                ogc1.Rows.Add(New Object() {comp.Name, False, comp.Name, comp.Tag, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
            Next

            Dim addobj As Boolean = True

            'property packages
            Me.DataGridViewPP.Rows.Clear()
            For Each pp2 As PropertyPackages.PropertyPackage In FormMain.PropertyPackages.Values.OrderBy(Function(x) x.ComponentName)
                If Not FrmChild.MobileCompatibilityMode Then
                    addobj = True
                Else
                    addobj = pp2.MobileCompatible
                End If
                If addobj Then Me.DataGridViewPP.Rows.Add(New String() {pp2.ComponentName, pp2.ComponentName, pp2.ComponentDescription})
            Next

            'flash algorithms
            Me.dgvAvailableFlashAlgos.Rows.Clear()
            For Each fa In FormMain.FlashAlgorithms.Values.OrderBy(Function(x) x.Order)
                If Not FrmChild.MobileCompatibilityMode Then
                    addobj = True
                Else
                    addobj = fa.MobileCompatible
                End If
                If addobj Then Me.dgvAvailableFlashAlgos.Rows.Add(New String() {fa.Name, fa.Description})
            Next


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
            If FrmChild.FlowsheetOptions.VisibleProperties.Count = 0 Then add = True

            cbObjectType.Items.Clear()
            availableproperties.Clear()
            aTypeRefs.Clear()
            For Each item In aTypeList.OrderBy(Function(x) x.Name)
                If Not item.IsAbstract Then
                    Dim obj = DirectCast(Activator.CreateInstance(item), Interfaces.ISimulationObject)
                    obj.SetFlowsheet(FrmChild)
                    cbObjectType.Items.Add(obj.GetDisplayName)
                    availableproperties.Add(obj.GetDisplayName, obj.GetProperties(PropertyType.ALL))
                    aTypeRefs.Add(obj.GetDisplayName, item.Name)
                    If add Then FrmChild.FlowsheetOptions.VisibleProperties.Add(item.Name, obj.GetDefaultProperties.ToList)
                    obj = Nothing
                End If
            Next
            cbObjectType.SelectedIndex = 0

            With Me.dgvpp.Rows
                .Clear()
                For Each pp2 As PropertyPackages.PropertyPackage In FrmChild.Options.PropertyPackages.Values
                    .Add(New Object() {pp2.UniqueID, pp2.Tag, pp2.ComponentName})
                Next
            End With

            With Me.dgvAddedFlashAlgos.Rows
                .Clear()
                For Each fa In FrmChild.Options.FlashAlgorithms
                    .Add(New Object() {fa.Tag, fa.Name})
                Next
            End With

            Me.ComboBox2.Items.Clear()
            Me.ComboBox2.Items.AddRange(FormMain.AvailableUnitSystems.Keys.ToArray)

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

        ComboBox1.SelectedItem = Me.FrmChild.Options.NumberFormat
        ComboBox3.SelectedItem = Me.FrmChild.Options.FractionNumberFormat

        If Me.FrmChild.Options.SelectedUnitSystem.Name <> "" Then
            ComboBox2.SelectedItem = Me.FrmChild.Options.SelectedUnitSystem.Name
        Else
            ComboBox2.SelectedIndex = 0
        End If

        Me.TBaut.Text = Me.FrmChild.Options.SimulationAuthor
        Me.TBdesc.Text = Me.FrmChild.Options.SimulationComments.Replace(vbLf, vbCrLf)
        Me.TBtit.Text = Me.FrmChild.Options.SimulationName

        Me.tbPassword.Text = FrmChild.Options.Password
        Me.chkUsePassword.Checked = FrmChild.Options.UsePassword

        If DWSIM.App.IsRunningOnMono Then btnConfigPP.Enabled = True

        cbMassBalanceCheck.SelectedIndex = FrmChild.Options.MassBalanceCheck

        cbEnergyBalanceCheck.SelectedIndex = FrmChild.Options.EnergyBalanceCheck

        tbMassBalTol.Text = FrmChild.Options.MassBalanceRelativeTolerance.ToString()

        tbEnergyBalTol.Text = FrmChild.Options.EnergyBalanceRelativeTolerance.ToString()

        chkShowFloatingTables.Checked = FrmChild.Options.DisplayFloatingPropertyTables
        chkShowAnchoredPropertyLists.Checked = FrmChild.Options.DisplayCornerPropertyList

        chkDisplayFloatingTableCompoundAmounts.Checked = FrmChild.Options.DisplayFloatingTableCompoundAmounts
        cbDefaultFloatingTableCompoundAmountBasis.SelectedIndex = FrmChild.Options.DefaultFloatingTableCompoundAmountBasis

        cbOrderCompoundsBy.SelectedIndex = FrmChild.Options.CompoundOrderingMode

        Me.loaded = True

    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBox1.SelectedIndexChanged
        FrmChild.Options.NumberFormat = Me.ComboBox1.SelectedItem
        FrmChild.UpdateOpenEditForms()
    End Sub

    Private Sub TBtit_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TBtit.TextChanged
        If Me.loaded Then
            Me.FrmChild.Options.SimulationName = Me.TBtit.Text
            Me.FrmChild.Text = Me.TBtit.Text + " (" + Me.FrmChild.Options.FilePath + ")"
        End If
    End Sub

    Private Sub TBaut_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TBaut.TextChanged
        If Me.loaded Then Me.FrmChild.Options.SimulationAuthor = Me.TBaut.Text
    End Sub

    Private Sub TBdesc_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TBdesc.TextChanged
        If Me.loaded Then Me.FrmChild.Options.SimulationComments = Me.TBdesc.Text
    End Sub

    Private Sub Button2_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Me.FrmChild.FrmPCBulk.ShowDialog(Me)
    End Sub

    Public Sub ComboBox2_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBox2.SelectedIndexChanged

        My.Application.PushUndoRedoAction = False

        FrmChild.Options.SelectedUnitSystem = FormMain.AvailableUnitSystems.Item(ComboBox2.SelectedItem.ToString)
        Dim su As SystemsOfUnits.Units = FrmChild.Options.SelectedUnitSystem

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

        FrmChild.UpdateOpenEditForms()

        My.Application.PushUndoRedoAction = True

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

            If initialized And Not DWSIM.App.IsRunningOnMono And My.Application.PushUndoRedoAction Then FrmChild.AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.SystemOfUnitsChanged,
                         .ObjID = su.Name,
                         .ObjID2 = member,
                         .NewValue = cell.Value,
                         .OldValue = oldvalue,
                         .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_SystemOfUnitsChanged"), su.Name, Me.DataGridView1.Rows(e.RowIndex).Cells(e.ColumnIndex - 1).Value, .OldValue, .NewValue)})

            Me.FrmChild.FormSurface.UpdateSelectedObject()

            FrmChild.UpdateOpenEditForms()

        End If

    End Sub

    Private Sub KryptonButton15_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton15.Click
        Dim frmUnit As New FormUnitGen
        frmUnit.ShowDialog(Me)
    End Sub

    Private Sub KryptonButton18_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton18.Click

        If Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("SistemaSI") And _
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("SistemaCGS") And _
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("SistemaIngls") And _
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("Personalizado1BR") And _
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("Personalizado2SC") And _
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("Personalizado3CNTP") Then

            Dim str = Me.ComboBox2.SelectedItem

            FrmChild.AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.SystemOfUnitsRemoved,
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

        If Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("SistemaSI") And _
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("SistemaCGS") And _
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("SistemaIngls") And _
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("Personalizado1BR") And _
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("Personalizado2SC") And _
            Me.ComboBox2.SelectedItem <> DWSIM.App.GetLocalString("Personalizado3CNTP") Then

            Dim myStream As System.IO.FileStream

            If Me.SaveFileDialog1.ShowDialog() = Windows.Forms.DialogResult.OK Then
                myStream = Me.SaveFileDialog1.OpenFile()
                If Not (myStream Is Nothing) Then
                    Dim su As SystemsOfUnits.Units = FrmChild.Options.SelectedUnitSystem
                    Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
                    Try
                        mySerializer.Serialize(myStream, su)
                    Catch ex As System.Runtime.Serialization.SerializationException
                        MessageBox.Show(ex.Message, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Finally
                        myStream.Close()
                    End Try
                End If
            End If

        Else
            MessageBox.Show(DWSIM.App.GetLocalString("EsteSistemadeUnidade"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
        End If

    End Sub

    Private Sub KryptonButton22_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton22.Click

        Dim myStream As System.IO.FileStream

        If Me.OpenFileDialog2.ShowDialog() = Windows.Forms.DialogResult.OK Then
            myStream = Me.OpenFileDialog2.OpenFile()
            If Not (myStream Is Nothing) Then
                Dim su As New SystemsOfUnits.Units
                Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
                Try
                    su = DirectCast(mySerializer.Deserialize(myStream), SystemsOfUnits.Units)
                    If FormMain.AvailableUnitSystems.ContainsKey(su.Name) Then
                        su.Name += "_1"
                    End If
                    FormMain.AvailableUnitSystems.Add(su.Name, su)
                    Me.ComboBox2.Items.Add(su.Name)
                    Me.FrmChild.Options.SelectedUnitSystem.Name = su.Name
                    Dim array1(FormMain.AvailableUnitSystems.Count - 1) As String
                    FormMain.AvailableUnitSystems.Keys.CopyTo(array1, 0)
                    ComboBox2.SelectedItem = Me.FrmChild.Options.SelectedUnitSystem.Name
                Catch ex As System.Runtime.Serialization.SerializationException
                    MessageBox.Show(ex.Message, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                Finally
                    myStream.Close()
                End Try
            End If
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


    Private Sub TextBox1_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBox1.TextChanged
        ogc1.ClearSelection()
        ogc1.SuspendLayout()
        Dim needselecting As Boolean = True
        For Each r As DataGridViewRow In ogc1.Rows
            If Not r.Cells(1).Value Is Nothing Then
                If r.Cells(1).Value.ToString.ToLower.Contains(Me.TextBox1.Text.ToLower) Or
                   r.Cells(2).Value.ToString.ToLower.Contains(Me.TextBox1.Text.ToLower) Or
                   r.Cells(4).Value.ToString.ToLower.Contains(Me.TextBox1.Text.ToLower) Then
                    r.Visible = True
                    If r.Cells(1).Value.ToString.Equals(Me.TextBox1.Text) Or
                    r.Cells(2).Value.ToString.Equals(Me.TextBox1.Text) Or
                    r.Cells(4).Value.ToString.Equals(Me.TextBox1.Text) Then
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
            ogc1.ResumeLayout()
            ogc1.FirstDisplayedScrollingRowIndex = 0
            Application.DoEvents()
            ogc1.Sort(colAdd, System.ComponentModel.ListSortDirection.Descending)
        Else
            ogc1.ResumeLayout()
        End If

    End Sub

    Private Sub btnConfigPP_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnConfigPP.Click
        Dim ppid As String = ""
        If DWSIM.App.IsRunningOnMono Then
            ppid = dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value
        Else
            ppid = dgvpp.SelectedRows(0).Cells(0).Value
        End If
        Dim pp As PropertyPackages.PropertyPackage = FrmChild.PropertyPackages(ppid)
        pp.DisplayEditingForm()
    End Sub

    Private Sub btnDeletePP_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnDeletePP.Click
        If DWSIM.App.IsRunningOnMono Then
            If dgvpp.SelectedCells.Count > 0 Then
                FrmChild.AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.PropertyPackageRemoved,
                          .ObjID = dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value,
                          .NewValue = FrmChild.Options.PropertyPackages(dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value).Clone,
                          .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_PropertyPackageRemoved"), dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(1).Value)})
                FrmChild.Options.PropertyPackages.Remove(dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value)
                dgvpp.Rows.RemoveAt(dgvpp.SelectedCells(0).RowIndex)
            End If
        Else
            If Not dgvpp.SelectedRows.Count = 0 Then
                FrmChild.AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.PropertyPackageRemoved,
                   .ObjID = dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value,
                   .NewValue = FrmChild.Options.PropertyPackages(dgvpp.SelectedRows(0).Cells(0).Value).Clone,
                   .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_PropertyPackageRemoved"), dgvpp.SelectedRows(0).Cells(1).Value)})
                FrmChild.Options.PropertyPackages.Remove(dgvpp.SelectedRows(0).Cells(0).Value)
                dgvpp.Rows.Remove(dgvpp.SelectedRows(0))
            End If
        End If
        If dgvpp.Rows.Count > 0 Then FrmChild.UpdateOpenEditForms()

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
            pp = FrmChild.Options.PropertyPackages(ppid).DeepClone
            pp.Flowsheet = FrmChild
            With pp
                pp.Tag = pp.Tag & CStr(FormFlowsheet.Options.PropertyPackages.Count)
                pp.UniqueID = Guid.NewGuid.ToString
            End With
            FrmChild.Options.PropertyPackages.Add(pp.UniqueID, pp)
            Me.dgvpp.Rows.Add(New Object() {pp.UniqueID, pp.Tag, pp.ComponentName})
            FrmChild.UpdateOpenEditForms()

        Catch ex As Exception

        End Try
    End Sub

    Private Sub dgvpp_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvpp.CellValueChanged
        If loaded Then
            FrmChild.Options.PropertyPackages(dgvpp.Rows(e.RowIndex).Cells(0).Value).Tag = dgvpp.Rows(e.RowIndex).Cells(1).Value
        End If
    End Sub

    Private Sub dgvpp_SelectionChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles dgvpp.SelectionChanged
        If DWSIM.App.IsRunningOnMono Then
            If dgvpp.SelectedCells.Count > 0 Then
                If dgvpp.SelectedCells(0).RowIndex >= 0 Then
                    If dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value <> Nothing Then
                        btnDeletePP.Enabled = True
                        If FrmChild.Options.PropertyPackages.ContainsKey(dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value) Then
                            btnConfigPPAdv.Enabled = True
                            If FrmChild.Options.PropertyPackages(dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value).IsConfigurable Then btnConfigPP.Enabled = True Else btnConfigPP.Enabled = False
                            btnCopyPP.Enabled = True
                        End If
                    End If
                End If
            End If
        Else
            If dgvpp.SelectedRows.Count > 0 Then
                btnDeletePP.Enabled = True
                If FrmChild.Options.PropertyPackages.ContainsKey(dgvpp.SelectedRows(0).Cells(0).Value) Then
                    btnConfigPPAdv.Enabled = True
                    If FrmChild.Options.PropertyPackages(dgvpp.SelectedRows(0).Cells(0).Value).IsConfigurable Then btnConfigPP.Enabled = True Else btnConfigPP.Enabled = False
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

        ' TODO Add code to check that index is within range. If it is out of range, don't do anything.
        If Me.loaded Then

            If Not Me.FrmChild.Options.SelectedComponents.ContainsKey(compid) Then

                Dim tmpcomp As New BaseClasses.ConstantProperties
                tmpcomp = Me.FrmChild.Options.NotSelectedComponents(compid)

                Me.FrmChild.Options.SelectedComponents.Add(tmpcomp.Name, tmpcomp)
                Me.FrmChild.Options.NotSelectedComponents.Remove(tmpcomp.Name)

                For Each mstr In FrmChild.Collections.FlowsheetObjectCollection.Values.Where(Function(x) TypeOf x Is Streams.MaterialStream)
                    For Each phase In mstr.Phases.Values
                        phase.Compounds.Add(tmpcomp.Name, New Compound(tmpcomp.Name, ""))
                        phase.Compounds(tmpcomp.Name).ConstantProperties = tmpcomp
                    Next
                Next

                FrmChild.UpdateOpenEditForms()

                If My.Application.PushUndoRedoAction Then FrmChild.AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.CompoundAdded,
                          .ObjID = tmpcomp.Name,
                          .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_CompoundAdded"), tmpcomp.Name)})


            End If


        End If

    End Sub

    Sub RemoveCompFromSimulation(ByVal compid As String)

        Dim tmpcomp As New BaseClasses.ConstantProperties
        Dim nm As String = compid
        tmpcomp = Me.FrmChild.Options.SelectedComponents(nm)
        Me.FrmChild.Options.SelectedComponents.Remove(tmpcomp.Name)
        Me.FrmChild.Options.NotSelectedComponents.Add(tmpcomp.Name, tmpcomp)
        Me.AddCompToGrid(tmpcomp)
        Dim ms As Streams.MaterialStream
        Dim proplist As New ArrayList

        For Each ms In FrmChild.Collections.FlowsheetObjectCollection.Values.Where(Function(x) TypeOf x Is Streams.MaterialStream)
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

        FrmChild.UpdateOpenEditForms()

        If My.Application.PushUndoRedoAction Then FrmChild.AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.CompoundRemoved,
          .ObjID = tmpcomp.Name,
          .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_CompoundRemoved"), tmpcomp.Name)})

    End Sub

    Private Sub ogc1_DataError(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles ogc1.DataError

    End Sub

    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click

        Dim pp As PropertyPackages.PropertyPackage
        pp = FormMain.PropertyPackages(Me.DataGridViewPP.SelectedRows(0).Cells(0).Value).Clone

        With pp
            pp.Tag = pp.ComponentName + " (" + (FrmChild.PropertyPackages.Count + 1).ToString() + ")"
            pp.UniqueID = "PP-" & Guid.NewGuid.ToString
            pp.Flowsheet = FrmChild
        End With

        FrmChild.Options.PropertyPackages.Add(pp.UniqueID, pp)
        Me.dgvpp.Rows.Add(New Object() {pp.UniqueID, pp.Tag, pp.ComponentName})

        FrmChild.UpdateOpenEditForms()

        FrmChild.AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.PropertyPackageAdded,
                                 .ObjID = pp.UniqueID,
                                 .NewValue = pp.Clone,
                                 .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_PropertyPackageAdded"), pp.Tag)})

    End Sub

    Private Sub ListViewPP_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DataGridViewPP.SelectionChanged
        If Me.DataGridViewPP.SelectedRows.Count > 0 Then
            Me.Button8.Enabled = True
        Else
            Me.Button8.Enabled = False
        End If
    End Sub

    Private Sub ComboBox3_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBox3.SelectedIndexChanged
        FrmChild.Options.FractionNumberFormat = Me.ComboBox3.SelectedItem
        FrmChild.UpdateOpenEditForms()
    End Sub

    Private Sub TextBox1_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TextBox1.KeyDown
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
        FrmChild.Options.UsePassword = chkUsePassword.Checked
    End Sub

    Private Sub tbPassword_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tbPassword.TextChanged
        FrmChild.Options.Password = tbPassword.Text
    End Sub

    Private Sub DataGridView1_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles DataGridView1.DataError

    End Sub

    Private Sub Button3_Click_2(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        Dim frmam As New FormAssayManager
        frmam.ShowDialog(Me)
        frmam.Close()
    End Sub

    Private Sub LinkLabelPropertyMethods_LinkClicked(sender As System.Object, e As System.Windows.Forms.LinkLabelLinkClickedEventArgs)
        Process.Start("http://dwsim.inforside.com.br/wiki/index.php?title=Property_Methods_and_Correlation_Profiles")
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
        TextBox1.Text = ""
    End Sub

    Private Sub cbObjectType_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbObjectType.SelectedIndexChanged
        PropertyListView.Items.Clear()
        For Each item In availableproperties(cbObjectType.SelectedItem)
            PropertyListView.Items.Add(item, DWSIM.App.GetPropertyName(item), 0).Tag = item
        Next

        For Each item In availableproperties(cbObjectType.SelectedItem)
            If FrmChild.FlowsheetOptions.VisibleProperties(aTypeRefs(cbObjectType.SelectedItem)).Contains(item) Then
                PropertyListView.Items(item).Checked = True
            End If
        Next

    End Sub

    Private Sub PropertyListView_ItemChecked(sender As Object, e As ItemCheckedEventArgs) Handles PropertyListView.ItemChecked
        If loaded Then
            If e.Item.Checked Then
                If Not FrmChild.FlowsheetOptions.VisibleProperties(aTypeRefs(cbObjectType.SelectedItem)).Contains(e.Item.Tag) Then
                    FrmChild.FlowsheetOptions.VisibleProperties(aTypeRefs(cbObjectType.SelectedItem)).Add(e.Item.Tag)
                End If
            Else
                If FrmChild.FlowsheetOptions.VisibleProperties(aTypeRefs(cbObjectType.SelectedItem)).Contains(e.Item.Tag) Then
                    FrmChild.FlowsheetOptions.VisibleProperties(aTypeRefs(cbObjectType.SelectedItem)).Remove(e.Item.Tag)
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


    Private Sub btnAddFA_Click(sender As Object, e As EventArgs) Handles btnAddFA.Click

        Dim fa As Auxiliary.FlashAlgorithms.FlashAlgorithm

        fa = FormMain.FlashAlgorithms.Values.Where(Function(x) x.Name = dgvAvailableFlashAlgos.SelectedRows(0).Cells(0).Value).FirstOrDefault

        fa.Tag = fa.Name & " (" & CStr(Me.dgvAddedFlashAlgos.Rows.Count + 1) & ")"

        FrmChild.Options.FlashAlgorithms.Add(fa)
        Me.dgvAddedFlashAlgos.Rows.Add(New Object() {fa.Tag, fa.Name})

        FrmChild.UpdateOpenEditForms()

    End Sub

    Private Sub btnConfigFA_Click(sender As Object, e As EventArgs) Handles btnConfigFA.Click

        Dim faid As String = ""
        If DWSIM.App.IsRunningOnMono Then
            faid = dgvAddedFlashAlgos.Rows(dgvAddedFlashAlgos.SelectedCells(0).RowIndex).Cells(0).Value
        Else
            faid = dgvAddedFlashAlgos.SelectedRows(0).Cells(0).Value
        End If
        Dim fa As Auxiliary.FlashAlgorithms.FlashAlgorithm = FrmChild.Options.FlashAlgorithms.Where(Function(x) x.Tag = faid).FirstOrDefault
        Dim f As New Thermodynamics.FlashAlgorithmConfig() With {.Settings = fa.FlashSettings,
                                                                .AvailableCompounds = Me.FrmChild.Options.SelectedComponents.Values.Select(Function(x) x.Name).ToList,
                                                                 .FlashAlgo = fa}

        If TypeOf fa Is Auxiliary.FlashAlgorithms.CAPEOPEN_Equilibrium_Server Then

            Dim coflash = DirectCast(fa, Auxiliary.FlashAlgorithms.CAPEOPEN_Equilibrium_Server)

            f._coes = coflash._coes
            f._coppm = coflash._coppm
            f._selppm = coflash._selppm
            f._esname = coflash._esname
            f._mappings = coflash._mappings
            f._phasemappings = coflash._phasemappings

            f.ShowDialog(Me)

            coflash._coes = f._coes
            coflash._coppm = f._coppm
            coflash._selppm = f._selppm
            coflash._esname = f._esname
            coflash._mappings = f._mappings
            coflash._phasemappings = f._phasemappings

            fa.FlashSettings = f.Settings

            f.Dispose()
            f = Nothing

        Else

            f.ShowDialog(Me)
            fa.FlashSettings = f.Settings
            f.Dispose()
            f = Nothing

        End If


    End Sub

    Private Sub btnCopyFA_Click(sender As Object, e As EventArgs) Handles btnCopyFA.Click

        Dim fa As Auxiliary.FlashAlgorithms.FlashAlgorithm

        Try
            Dim faid As String = ""
            If DWSIM.App.IsRunningOnMono Then
                faid = dgvAddedFlashAlgos.Rows(dgvAddedFlashAlgos.SelectedCells(0).RowIndex).Cells(0).Value
            Else
                faid = dgvAddedFlashAlgos.SelectedRows(0).Cells(0).Value
            End If
            fa = FrmChild.Options.FlashAlgorithms.Where(Function(x) x.Tag = faid).FirstOrDefault.Clone
            fa.Tag = fa.Tag & "_Clone"

            FrmChild.Options.FlashAlgorithms.Add(fa)
            Me.dgvAddedFlashAlgos.Rows.Add(New Object() {fa.Tag, fa.Name})

            FrmChild.UpdateOpenEditForms()

        Catch ex As Exception

        End Try

    End Sub

    Private Sub btnDeleteFA_Click(sender As Object, e As EventArgs) Handles btnDeleteFA.Click

        If DWSIM.App.IsRunningOnMono Then
            If dgvAddedFlashAlgos.SelectedCells.Count > 0 Then
                Dim fa As Auxiliary.FlashAlgorithms.FlashAlgorithm = FrmChild.Options.FlashAlgorithms.Where(Function(x) x.Tag = dgvAddedFlashAlgos.Rows(dgvAddedFlashAlgos.SelectedCells(0).RowIndex).Cells(0).Value).FirstOrDefault
                FrmChild.Options.FlashAlgorithms.Remove(fa)
                dgvAddedFlashAlgos.Rows.RemoveAt(dgvAddedFlashAlgos.SelectedCells(0).RowIndex)
            End If
        Else
            If Not dgvpp.SelectedRows.Count = 0 Then
                Dim fa As Auxiliary.FlashAlgorithms.FlashAlgorithm = FrmChild.Options.FlashAlgorithms.Where(Function(x) x.Tag = dgvAddedFlashAlgos.SelectedRows(0).Cells(0).Value).FirstOrDefault
                FrmChild.Options.FlashAlgorithms.Remove(fa)
                dgvAddedFlashAlgos.Rows.Remove(dgvAddedFlashAlgos.SelectedRows(0))
            End If
        End If

        FrmChild.UpdateOpenEditForms()

    End Sub

    Private Sub dgvAvailableFlashAlgos_DoubleClick(sender As Object, e As EventArgs) Handles dgvAvailableFlashAlgos.DoubleClick
        If Me.DataGridViewPP.SelectedRows.Count = 1 Then
            btnAddFA.PerformClick()
        End If
    End Sub

    Private Sub dgvAddedFlashAlgos_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles dgvAddedFlashAlgos.CellValueChanged
        If loaded Then
            FrmChild.Options.FlashAlgorithms(e.RowIndex).Tag = dgvAddedFlashAlgos.Rows(e.RowIndex).Cells(0).Value
        End If
    End Sub

    Private Sub btnInfoLeft_Click(sender As Object, e As EventArgs) Handles btnInfoLeft.Click
        Dim compound As Interfaces.ICompoundConstantProperties
        Dim compID As String = ""
        If DWSIM.App.IsRunningOnMono Then
            compID = ogc1.Rows(ogc1.SelectedCells(0).RowIndex).Cells(0).Value
        Else
            compID = ogc1.SelectedRows(0).Cells(0).Value
        End If
        If FrmChild.AvailableCompounds.ContainsKey(compID) Then
            compound = Me.FrmChild.AvailableCompounds(compID)
        ElseIf FrmChild.Options.SelectedComponents.ContainsKey(compID) Then
            compound = Me.FrmChild.Options.SelectedComponents(compID)
        ElseIf FrmChild.Options.NotSelectedComponents.ContainsKey(compID) Then
            compound = Me.FrmChild.Options.NotSelectedComponents(compID)
        Else
            compound = Nothing
        End If
        Dim f As New FormPureComp() With {.Flowsheet = FrmChild, .Added = False, .MyCompound = compound}
        FrmChild.DisplayForm(f)
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
        If Me.OpenFileDialog3.ShowDialog(Me) = Windows.Forms.DialogResult.OK Then
            For Each fn In Me.OpenFileDialog3.FileNames
                Try
                    Dim comp = Newtonsoft.Json.JsonConvert.DeserializeObject(Of BaseClasses.ConstantProperties)(File.ReadAllText(fn))
                    If Not Me.FrmChild.Options.SelectedComponents.ContainsKey(comp.Name) Then
                        Me.FrmChild.Options.SelectedComponents.Add(comp.Name, comp)
                        Dim ms As Streams.MaterialStream
                        Dim proplist As New ArrayList
                        For Each ms In FrmChild.Collections.FlowsheetObjectCollection.Values.Where(Function(x) TypeOf x Is Streams.MaterialStream)
                            For Each phase As BaseClasses.Phase In ms.Phases.Values
                                phase.Compounds.Add(comp.Name, New BaseClasses.Compound(comp.Name, ""))
                                phase.Compounds(comp.Name).ConstantProperties = comp
                            Next
                        Next
                        ogc1.Rows.Add(New Object() {comp.Name, True, comp.Name, comp.Tag, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
                        ogc1.ClearSelection()
                        ogc1.Rows(ogc1.Rows.Count - 1).Selected = True
                        ogc1.FirstDisplayedScrollingRowIndex = ogc1.Rows.Count - 1
                    Else
                        MessageBox.Show(DWSIM.App.GetLocalString("CompoundExists"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End If
                Catch ex As Exception
                    MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End Try
            Next
        End If
    End Sub

    Private Sub Button13_Click(sender As Object, e As EventArgs) Handles Button13.Click
        Dim f As New FormImportCompoundOnline
        If f.ShowDialog(Me) = Windows.Forms.DialogResult.OK Then
            Try
                Dim comp = f.BaseCompound
                If Not Me.FrmChild.Options.SelectedComponents.ContainsKey(comp.Name) Then
                    Me.FrmChild.Options.SelectedComponents.Add(comp.Name, comp)
                    Dim ms As Streams.MaterialStream
                    Dim proplist As New ArrayList
                    For Each ms In FrmChild.Collections.FlowsheetObjectCollection.Values.Where(Function(x) TypeOf x Is Streams.MaterialStream)
                        For Each phase As BaseClasses.Phase In ms.Phases.Values
                            phase.Compounds.Add(comp.Name, New BaseClasses.Compound(comp.Name, ""))
                            phase.Compounds(comp.Name).ConstantProperties = comp
                        Next
                    Next
                    ogc1.Rows.Add(New Object() {comp.Name, True, comp.Name, comp.Tag, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
                Else
                    MessageBox.Show(DWSIM.App.GetLocalString("CompoundExists"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End If
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If
    End Sub

    Private Sub cbMassBalanceCheck_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbMassBalanceCheck.SelectedIndexChanged
        FrmChild.Options.MassBalanceCheck = cbMassBalanceCheck.SelectedIndex
    End Sub

    Private Sub cbEnergyBalanceCheck_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbEnergyBalanceCheck.SelectedIndexChanged
        FrmChild.Options.EnergyBalanceCheck = cbEnergyBalanceCheck.SelectedIndex
    End Sub

    Private Sub tbMassBalTol_TextChanged(sender As Object, e As EventArgs) Handles tbMassBalTol.TextChanged
        If tbMassBalTol.Text.IsValidDouble Then FrmChild.Options.MassBalanceRelativeTolerance = tbMassBalTol.Text.ToDoubleFromCurrent
    End Sub

    Private Sub tbEnergyBalTol_TextChanged(sender As Object, e As EventArgs) Handles tbEnergyBalTol.TextChanged
        If tbEnergyBalTol.Text.IsValidDouble Then FrmChild.Options.EnergyBalanceRelativeTolerance = tbEnergyBalTol.Text.ToDoubleFromCurrent
    End Sub

    Private Sub chkShowFloatingTables_CheckedChanged(sender As Object, e As EventArgs) Handles chkShowFloatingTables.CheckedChanged
        FrmChild.Options.DisplayFloatingPropertyTables = chkShowFloatingTables.Checked
        FrmChild.FormSurface.FlowsheetSurface.DrawFloatingTable = chkShowFloatingTables.Checked
    End Sub

    Private Sub chkShowAnchoredPropertyLists_CheckedChanged(sender As Object, e As EventArgs) Handles chkShowAnchoredPropertyLists.CheckedChanged
        FrmChild.Options.DisplayCornerPropertyList = chkShowAnchoredPropertyLists.Checked
        FrmChild.FormSurface.FlowsheetSurface.DrawPropertyList = chkShowAnchoredPropertyLists.Checked
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click

        FontDialog1.Font = New Font(FrmChild.Options.DisplayCornerPropertyListFontName, FrmChild.Options.DisplayCornerPropertyListFontSize, FontStyle.Bold)

        Dim colortype = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("System.Drawing,")).FirstOrDefault().GetType("System.Drawing.KnownColor")
        Dim mycolor = [Enum].Parse(colortype, FrmChild.Options.DisplayCornerPropertyListFontColor, True)

        FontDialog1.Color = Color.FromKnownColor(mycolor)
        FontDialog1.ShowColor = True
        FontDialog1.ShowDialog(Me)

        FrmChild.Options.DisplayCornerPropertyListFontName = FontDialog1.Font.FontFamily.Name
        FrmChild.Options.DisplayCornerPropertyListFontColor = FontDialog1.Color.Name
        FrmChild.Options.DisplayCornerPropertyListFontSize = FontDialog1.Font.Size

    End Sub

    Private Sub chkDisplayFloatingTableCompoundAmounts_CheckedChanged(sender As Object, e As EventArgs) Handles chkDisplayFloatingTableCompoundAmounts.CheckedChanged
        FrmChild.Options.DisplayFloatingTableCompoundAmounts = chkDisplayFloatingTableCompoundAmounts.Checked
    End Sub

    Private Sub cbDefaultFloatingTableCompoundAmountBasis_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbDefaultFloatingTableCompoundAmountBasis.SelectedIndexChanged
        FrmChild.Options.DefaultFloatingTableCompoundAmountBasis = cbDefaultFloatingTableCompoundAmountBasis.SelectedIndex
    End Sub

    Private Sub btnConfigPPAdv_Click(sender As Object, e As EventArgs) Handles btnConfigPPAdv.Click
        Dim ppid As String = ""
        If GlobalSettings.Settings.IsRunningOnMono Then
            ppid = dgvpp.Rows(dgvpp.SelectedCells(0).RowIndex).Cells(0).Value
        Else
            ppid = dgvpp.SelectedRows(0).Cells(0).Value
        End If
        Dim pp As PropertyPackages.PropertyPackage = FrmChild.Options.PropertyPackages(ppid)

        pp.DisplayAdvancedEditingForm()

        'start dispatcher for WPF Interop
        If Not GlobalSettings.Settings.IsRunningOnMono Then System.Windows.Threading.Dispatcher.Run()

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

                If FrmChild.Options.SelectedComponents.ContainsKey(cid) Then
                    FrmChild.Options.SelectedComponents(cid).Tag = ogc1.Rows(e.RowIndex).Cells(colTag.Index).Value
                Else
                    FrmChild.Options.NotSelectedComponents(cid).Tag = ogc1.Rows(e.RowIndex).Cells(colTag.Index).Value
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
        FrmChild.Options.CompoundOrderingMode = cbOrderCompoundsBy.SelectedIndex
        FrmChild.UpdateOpenEditForms()
    End Sub

    Private Sub BtnCloneSI_Click(sender As Object, e As EventArgs) Handles btnCloneSI.Click

        Dim newsu = Newtonsoft.Json.JsonConvert.DeserializeObject(Of SystemsOfUnits.Units)(Newtonsoft.Json.JsonConvert.SerializeObject(FrmChild.Options.SelectedUnitSystem))

        Dim cnt As Integer = 1
        While FormMain.AvailableUnitSystems.ContainsKey(newsu.Name)
            newsu.Name = FrmChild.Options.SelectedUnitSystem.Name & "_" & cnt.ToString
            cnt += 1
        End While

        FrmChild.AddUnitSystem(newsu)
        FrmChild.AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.SystemOfUnitsAdded,
                                             .NewValue = newsu,
                                             .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_SystemOfUnitsAdded"), newsu.Name)})

        ComboBox2.SelectedIndex = ComboBox2.Items.Count - 1

    End Sub

    Private Sub FormSimulSettings_Shown(sender As Object, e As EventArgs) Handles Me.Shown

        ogc1.Sort(ogc1.Columns(1), System.ComponentModel.ListSortDirection.Descending)

    End Sub

End Class
