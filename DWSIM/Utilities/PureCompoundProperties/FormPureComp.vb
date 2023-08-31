Imports com.ggasoftware.indigo
Imports DWSIM.Thermodynamics.BaseClasses
Imports System.IO
Imports DWSIM.SharedClasses
Imports DWSIM.Thermodynamics.Streams
Imports System.Linq
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker

'    Copyright 2008-2014 Daniel Wagner O. de Medeiros
'              2013-2014 Gregor Reichert
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

Public Class FormPureComp

    Inherits Form

    Dim MatStream As Streams.MaterialStream

    Public Flowsheet As FormFlowsheet
    Public SUnits As SystemsOfUnits.Units

    Public MyCompound As ConstantProperties

    Public Added As Boolean = False
    Public Loaded As Boolean = False

    Private ind As Indigo
    Private mol As IndigoObject
    Private renderer As IndigoRenderer

    Dim vxCp, vyCp, vxPvap, vyPvap, vxVisc, vyVisc, vxDHvap, vyDHvap, vxLD, vyLD, vxSD, vySD, vxSCP, vySCP, vxVapVisc,
        vyVapVisc, vxVapThCond, vyVapThCond, vxLiqThCond, vyLiqThCond, vxSurfTens, vySurfTens, vxLiqCp, vyLiqCp As New ArrayList

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        SaveFileDialog2.FileName = MyCompound.Name
        If Me.SaveFileDialog2.ShowDialog(Me) = DialogResult.OK Then
            Try
                MyCompound.ExportToXLSX(SaveFileDialog2.FileName)
                MessageBox.Show(DWSIM.App.GetLocalString("FileSaved"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Information)
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erroaosalvararquivo") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If
    End Sub

    Public constprop As BaseClasses.ConstantProperties

    Private Sub FormPureComp_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        If FormMain.IsPro Then
            Button2.Enabled = False
            Button3.Enabled = False
        End If

        If Not MyCompound Is Nothing Then Me.Text = Me.Text & " - " & MyCompound.Name

    End Sub

    Sub Populate()

        Dim su As SystemsOfUnits.Units = SUnits
        Dim cv As New SystemsOfUnits.Converter
        Dim nf As String = "G6"
        Dim pp As New PropertyPackages.RaoultPropertyPackage(False)

        If Flowsheet IsNot Nothing Then
            su = Flowsheet.Options.SelectedUnitSystem
            nf = Flowsheet.Options.NumberFormat
        End If

        Me.MatStream = New MaterialStream("", "", Flowsheet, pp)

        'add compound to the dummy material stream
        For Each phase As BaseClasses.Phase In MatStream.Phases.Values
            With phase
                .Compounds.Add(MyCompound.Name, New BaseClasses.Compound(MyCompound.Name, ""))
                .Compounds(MyCompound.Name).ConstantProperties = MyCompound
            End With
        Next
        DirectCast(MatStream, Streams.MaterialStream).EqualizeOverallComposition()

        pp.CurrentMaterialStream = MatStream

        'setting up datatable
        Dim Row As Integer
        Dim TD, VD As Double

        'setting up liquid table
        Me.DataTableLiquid.Rows.Clear()
        Me.DataTableLiquid.Rows.Add(51)
        Me.DataTableLiquid.Columns.Item(0).HeaderText = "Temp " & su.temperature
        Me.DataTableLiquid.Columns.Item(2).HeaderText = "Temp " & su.temperature
        Me.DataTableLiquid.Columns.Item(4).HeaderText = "Temp " & su.temperature
        Me.DataTableLiquid.Columns.Item(6).HeaderText = "Temp " & su.temperature
        Me.DataTableLiquid.Columns.Item(8).HeaderText = "Temp " & su.temperature
        Me.DataTableLiquid.Columns.Item(10).HeaderText = "Temp " & su.temperature
        Me.DataTableLiquid.Columns.Item(12).HeaderText = "Temp " & su.temperature
        Me.DataTableLiquid.Columns.Item(1).HeaderText = DWSIM.App.GetLocalString("CapacidadeCalorfica") & " " & su.heatCapacityCp
        Me.DataTableLiquid.Columns.Item(3).HeaderText = DWSIM.App.GetLocalString("EntalpiadeVaporizao") & " " & su.enthalpy
        Me.DataTableLiquid.Columns.Item(5).HeaderText = DWSIM.App.GetLocalString("PressodeVapor") & " " & su.pressure
        Me.DataTableLiquid.Columns.Item(7).HeaderText = DWSIM.App.GetLocalString("Tensosuperficial") & " " & su.surfaceTension
        Me.DataTableLiquid.Columns.Item(9).HeaderText = DWSIM.App.GetLocalString("ViscosidadeLquido") & " " & su.viscosity
        Me.DataTableLiquid.Columns.Item(11).HeaderText = DWSIM.App.GetLocalString("LiquidDensity") & " " & su.density
        Me.DataTableLiquid.Columns.Item(13).HeaderText = DWSIM.App.GetLocalString("Condutividadetrmica") & " " & su.thermalConductivity

        'setting up vapour table
        Me.DataTableVapour.Rows.Clear()
        Me.DataTableVapour.Rows.Add(51)
        Me.DataTableVapour.Columns.Item(0).HeaderText = "Temp " & su.temperature
        Me.DataTableVapour.Columns.Item(2).HeaderText = "Temp " & su.temperature
        Me.DataTableVapour.Columns.Item(4).HeaderText = "Temp " & su.temperature
        Me.DataTableVapour.Columns.Item(1).HeaderText = DWSIM.App.GetLocalString("CapacidadeCalorfica") & " " & su.heatCapacityCp
        Me.DataTableVapour.Columns.Item(3).HeaderText = DWSIM.App.GetLocalString("ViscosidadeLquido") & " " & su.viscosity
        Me.DataTableVapour.Columns.Item(5).HeaderText = DWSIM.App.GetLocalString("Condutividadetrmica") & " " & su.thermalConductivity

        'setting up solid table
        Me.DataTableSolid.Rows.Clear()
        Me.DataTableSolid.Rows.Add(51)
        Me.DataTableSolid.Columns.Item(0).HeaderText = "Temp " & su.temperature
        Me.DataTableSolid.Columns.Item(2).HeaderText = "Temp " & su.temperature
        Me.DataTableSolid.Columns.Item(1).HeaderText = DWSIM.App.GetLocalString("SolidDensity") & " " & su.density
        Me.DataTableSolid.Columns.Item(3).HeaderText = DWSIM.App.GetLocalString("SolidCp") & " " & su.heatCapacityCp

        If Not constprop.IsBlackOil Then

            'setting up curves
            Dim T As Double
            Dim Tmin, Tmax, delta As Double


            '======== vapour properties ===================================================================

            'ideal gas heat capacity
            Tmin = 200
            Tmax = 1500
            delta = (Tmax - Tmin) / 50
            T = Tmin
            Row = 0
            vxCp.Clear()
            vyCp.Clear()

            If Not constprop.IsIon And Not constprop.IsSalt Then
                Do
                    TD = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                    VD = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, pp.AUX_CPi(constprop.Name, T))
                    vxCp.Add(TD)
                    vyCp.Add(VD)
                    Me.DataTableVapour.Item(0, Row).Value = Format(TD, nf)
                    Me.DataTableVapour.Item(1, Row).Value = Format(VD, nf)
                    T += delta
                    Row += 1
                Loop Until T > Tmax
            End If

            With Me.GraphCp.GraphPane
                .CurveList.Clear()
                With .AddCurve("", Me.vxCp.ToArray(GetType(Double)), Me.vyCp.ToArray(GetType(Double)), Color.Blue, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
                .Title.IsVisible = False
                .XAxis.Title.Text = "T [ " & su.temperature & " ] "
                .YAxis.Title.Text = "Cp [ " & su.heatCapacityCp & " ] "
                .AxisChange(Me.CreateGraphics)
            End With
            Me.GraphCp.Invalidate()


            'vapor viscosity
            With constprop
                If .Vapor_Viscosity_Tmin <> 0 Then
                    Tmin = 0.6 * .Vapor_Viscosity_Tmin
                    Tmax = .Vapor_Viscosity_Tmax
                Else
                    Tmin = 0.6 * .Critical_Temperature
                    Tmax = .Critical_Temperature
                End If
                delta = (Tmax - Tmin) / 50
            End With
            T = Tmin
            Row = 0
            vxVapVisc.Clear()
            vyVapVisc.Clear()
            If Not constprop.IsIon And Not constprop.IsSalt Then
                Do
                    TD = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                    VD = SystemsOfUnits.Converter.ConvertFromSI(su.viscosity, pp.AUX_VAPVISCi(constprop, T))
                    vxVapVisc.Add(TD)
                    vyVapVisc.Add(VD)
                    Me.DataTableVapour.Item(2, Row).Value = Format(TD, nf)
                    Me.DataTableVapour.Item(3, Row).Value = Format(VD, nf)
                    T += delta
                    Row += 1
                Loop Until Row = 51
            End If

            With Me.GraphVapVisc.GraphPane
                .CurveList.Clear()
                With .AddCurve("", Me.vxVapVisc.ToArray(GetType(Double)), Me.vyVapVisc.ToArray(GetType(Double)), Color.Blue, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
                .Title.IsVisible = False
                .XAxis.Title.Text = "T [ " & su.temperature & " ] "
                .YAxis.Title.Text = "Visc [ " & su.viscosity & " ] "
                .AxisChange(Me.CreateGraphics)
            End With
            Me.GraphVapVisc.Invalidate()


            'vapor thermal conductivity
            With constprop
                Tmin = .Vapor_Thermal_Conductivity_Tmin
                Tmax = .Vapor_Thermal_Conductivity_Tmax
                If Tmin = 0 Then Tmin = .Normal_Boiling_Point
                If Tmax = 0 Then Tmax = .Critical_Temperature
                delta = (Tmax - Tmin) / 50
            End With
            T = Tmin
            Row = 0
            vxVapThCond.Clear()
            vyVapThCond.Clear()
            If Not constprop.IsIon And Not constprop.IsSalt Then
                Do
                    TD = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                    VD = SystemsOfUnits.Converter.ConvertFromSI(su.thermalConductivity, pp.AUX_VAPTHERMCONDi(constprop, T, 101325))
                    vxVapThCond.Add(TD)
                    vyVapThCond.Add(VD)
                    Me.DataTableVapour.Item(4, Row).Value = Format(TD, nf)
                    Me.DataTableVapour.Item(5, Row).Value = Format(VD, nf)
                    T += delta
                    Row += 1
                Loop Until Row = 51
            End If

            With Me.GraphVapThermCond.GraphPane
                .CurveList.Clear()
                With .AddCurve("", Me.vxVapThCond.ToArray(GetType(Double)), Me.vyVapThCond.ToArray(GetType(Double)), Color.Blue, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
                .Title.IsVisible = False
                .XAxis.Title.Text = "T [ " & su.temperature & " ] "
                .YAxis.Title.Text = DWSIM.App.GetLocalString("CondutividadetrmicaF3") & " [ " & su.thermalConductivity & " ] "
                .AxisChange(Me.CreateGraphics)
            End With
            Me.GraphVapThermCond.Invalidate()


            '======== liquid properties ===================================================================

            'liquid heat capacity
            With constprop
                Tmin = .Liquid_Heat_Capacity_Tmin
                Tmax = .Liquid_Heat_Capacity_Tmax
                If Tmin = 0 Then Tmin = .TemperatureOfFusion
                If Tmin = 0 Then Tmin = .Normal_Boiling_Point * 0.6
                If Tmax = 0 Then Tmax = .Normal_Boiling_Point * 0.99
                delta = (Tmax - Tmin) / 50
            End With
            T = Tmin
            Row = 0
            vxLiqCp.Clear()
            vyLiqCp.Clear()

            If Not constprop.IsIon And Not constprop.IsSalt Then
                Do
                    TD = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                    VD = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, pp.AUX_LIQ_Cpi(constprop, T))
                    vxLiqCp.Add(TD)
                    vyLiqCp.Add(VD)
                    Me.DataTableLiquid.Item(0, Row).Value = Format(TD, nf)
                    Me.DataTableLiquid.Item(1, Row).Value = Format(VD, nf)
                    T += delta
                    Row += 1
                Loop Until T > Tmax
            End If

            With Me.GraphLiqCp.GraphPane
                .CurveList.Clear()
                With .AddCurve("", Me.vxLiqCp.ToArray(GetType(Double)), Me.vyLiqCp.ToArray(GetType(Double)), Color.Blue, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
                .Title.IsVisible = False
                .XAxis.Title.Text = "T [ " & su.temperature & " ] "
                .YAxis.Title.Text = "Cp [ " & su.heatCapacityCp & " ] "
                .AxisChange(Me.CreateGraphics)
            End With
            Me.GraphLiqCp.Invalidate()

            'vaporization enthalpy
            With constprop
                Tmin = .HVap_TMIN
                Tmax = .HVap_TMAX
                If Tmin = 0 Then Tmin = 0.6 * .Critical_Temperature
                If Tmax = 0 Then Tmax = .Critical_Temperature * 0.999
                delta = (Tmax - Tmin) / 50
            End With
            T = Tmin
            Row = 0
            vxDHvap.Clear()
            vyDHvap.Clear()
            If Not constprop.IsIon And Not constprop.IsSalt Then
                Do
                    TD = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                    VD = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, pp.AUX_HVAPi(constprop.Name, T))
                    vxDHvap.Add(TD)
                    vyDHvap.Add(VD)
                    Me.DataTableLiquid.Item(2, Row).Value = Format(TD, nf)
                    Me.DataTableLiquid.Item(3, Row).Value = Format(VD, nf)
                    T += delta
                    Row += 1
                Loop Until Row = 51
            End If

            With Me.GraphDHVAP.GraphPane
                .CurveList.Clear()
                With .AddCurve("", Me.vxDHvap.ToArray(GetType(Double)), Me.vyDHvap.ToArray(GetType(Double)), Color.Blue, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
                .Title.IsVisible = False
                .XAxis.Title.Text = "T [ " & su.temperature & " ] "
                .YAxis.Title.Text = "DHvap [ " & su.enthalpy & " ] "
                .AxisChange(Me.CreateGraphics)
            End With
            Me.GraphDHVAP.Invalidate()


            'vapor pressure
            With constprop
                Tmin = .Vapor_Pressure_TMIN
                Tmax = .Vapor_Pressure_TMAX
                If Tmin = 0 Then Tmin = 0.4 * .Critical_Temperature
                If Tmax = 0 Then Tmax = .Critical_Temperature
                delta = (Tmax - Tmin) / 50
            End With
            T = Tmin
            Row = 0
            vxPvap.Clear()
            vyPvap.Clear()
            If Not constprop.IsIon And Not constprop.IsSalt Then
                Do
                    TD = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                    VD = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, pp.AUX_PVAPi(constprop.Name, T))
                    vxPvap.Add(TD)
                    vyPvap.Add(VD)
                    Me.DataTableLiquid.Item(4, Row).Value = Format(TD, nf)
                    Me.DataTableLiquid.Item(5, Row).Value = Format(VD, nf)
                    T += delta
                    Row += 1
                Loop Until Row = 51
            End If

            With Me.GraphPvap.GraphPane
                .CurveList.Clear()
                With .AddCurve("", Me.vxPvap.ToArray(GetType(Double)), Me.vyPvap.ToArray(GetType(Double)), Color.Blue, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
                .Title.IsVisible = False
                .XAxis.Title.Text = "T [ " & su.temperature & " ] "
                .YAxis.Title.Text = "Pvap [ " & su.pressure & " ] "
                .YAxis.Type = ZedGraph.AxisType.Log
                .AxisChange(Me.CreateGraphics)
            End With
            Me.GraphPvap.Invalidate()


            'liquid surface tension
            With constprop
                Tmin = .Surface_Tension_Tmin
                Tmax = .Surface_Tension_Tmax
                If Tmin = 0 Then Tmin = .TemperatureOfFusion
                If Tmin = 0 Then Tmin = .Normal_Boiling_Point * 0.6
                If Tmax = 0 Then Tmax = .Normal_Boiling_Point * 0.999
                delta = (Tmax - Tmin) / 50
            End With
            T = Tmin
            Row = 0
            vxSurfTens.Clear()
            vySurfTens.Clear()
            If Not constprop.IsIon And Not constprop.IsSalt Then
                Do
                    TD = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                    VD = SystemsOfUnits.Converter.ConvertFromSI(su.surfaceTension, pp.AUX_SURFTi(constprop, T))
                    vxSurfTens.Add(TD)
                    vySurfTens.Add(VD)
                    Me.DataTableLiquid.Item(6, Row).Value = Format(TD, nf)
                    Me.DataTableLiquid.Item(7, Row).Value = Format(VD, nf)
                    T += delta
                    Row += 1
                Loop Until Row = 51
            End If

            With Me.GraphSurfT.GraphPane
                .CurveList.Clear()
                With .AddCurve("", Me.vxSurfTens.ToArray(GetType(Double)), Me.vySurfTens.ToArray(GetType(Double)), Color.Blue, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
                .Title.IsVisible = False
                .XAxis.Title.Text = "T [ " & su.temperature & " ] "
                .YAxis.Title.Text = DWSIM.App.GetLocalString("Tensosuperficial") & " [ " & su.surfaceTension & " ] "
                .AxisChange(Me.CreateGraphics)
            End With
            Me.GraphSurfT.Invalidate()


            'liquid viscosity
            With constprop
                Tmin = .TemperatureOfFusion
                If Tmin = 0 Then Tmin = .Normal_Boiling_Point * 0.6
                If Tmax = 0 Then Tmax = .Normal_Boiling_Point * 0.999
                delta = (Tmax - Tmin) / 50
            End With
            T = Tmin
            Row = 0
            vxVisc.Clear()
            vyVisc.Clear()
            If Not constprop.IsIon And Not constprop.IsSalt Then
                Do
                    TD = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                    VD = SystemsOfUnits.Converter.ConvertFromSI(su.viscosity, pp.AUX_LIQVISCi(constprop.Name, T, 101325))
                    vxVisc.Add(TD)
                    vyVisc.Add(VD)
                    Me.DataTableLiquid.Item(8, Row).Value = Format(TD, nf)
                    Me.DataTableLiquid.Item(9, Row).Value = Format(VD, nf)
                    T += delta
                    Row += 1
                Loop Until Row = 51
            End If

            With Me.GraphVisc.GraphPane
                .CurveList.Clear()
                With .AddCurve("", Me.vxVisc.ToArray(GetType(Double)), Me.vyVisc.ToArray(GetType(Double)), Color.Blue, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
                .Title.IsVisible = False
                .XAxis.Title.Text = "T [ " & su.temperature & " ] "
                .YAxis.Title.Text = "Visc [ " & su.viscosity & " ] "
                .AxisChange(Me.CreateGraphics)
            End With
            Me.GraphVisc.Invalidate()


            'liquid density
            With constprop
                Tmin = .Liquid_Density_Tmin
                Tmax = .Liquid_Density_Tmax
                If Tmin = 0 Then Tmin = .TemperatureOfFusion
                If Tmin = 0 Then Tmin = .Normal_Boiling_Point * 0.6
                If Tmax = 0 Then Tmax = .Normal_Boiling_Point * 0.999
                delta = (Tmax - Tmin) / 50
            End With
            T = Tmin
            Row = 0
            vxLD.Clear()
            vyLD.Clear()
            If Not constprop.IsIon And Not constprop.IsSalt Then
                Do
                    TD = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                    VD = SystemsOfUnits.Converter.ConvertFromSI(su.density, pp.AUX_LIQDENSi(constprop, T))
                    vxLD.Add(TD)
                    vyLD.Add(VD)
                    Me.DataTableLiquid.Item(10, Row).Value = Format(TD, nf)
                    Me.DataTableLiquid.Item(11, Row).Value = Format(VD, nf)
                    T += delta
                    Row += 1
                Loop Until Row = 51
            End If

            With Me.GraphLiqDens.GraphPane
                .CurveList.Clear()
                With .AddCurve("", Me.vxLD.ToArray(GetType(Double)), Me.vyLD.ToArray(GetType(Double)), Color.Blue, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
                .Title.IsVisible = False
                .XAxis.Title.Text = "T [ " & su.temperature & " ] "
                .YAxis.Title.Text = DWSIM.App.GetLocalString("LiquidDensity") & " [ " & su.density & " ] "
                .AxisChange(Me.CreateGraphics)
            End With
            Me.GraphLiqDens.Invalidate()

            'liquid thermal conductivity
            With constprop
                Tmin = .Liquid_Thermal_Conductivity_Tmin
                Tmax = .Liquid_Thermal_Conductivity_Tmax
                If Tmin = 0 Then Tmin = .TemperatureOfFusion
                If Tmin = 0 Then Tmin = .Normal_Boiling_Point * 0.6
                If Tmax = 0 Then Tmax = .Normal_Boiling_Point * 0.999
                delta = (Tmax - Tmin) / 50
            End With
            T = Tmin
            Row = 0
            vxLiqThCond.Clear()
            vyLiqThCond.Clear()
            If Not constprop.IsIon And Not constprop.IsSalt Then
                Do
                    TD = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                    VD = SystemsOfUnits.Converter.ConvertFromSI(su.thermalConductivity, pp.AUX_LIQTHERMCONDi(constprop, T))
                    vxLiqThCond.Add(TD)
                    vyLiqThCond.Add(VD)
                    Me.DataTableLiquid.Item(12, Row).Value = Format(TD, nf)
                    Me.DataTableLiquid.Item(13, Row).Value = Format(VD, nf)
                    T += delta
                    Row += 1
                Loop Until Row = 51
            End If

            With Me.GraphLiqThermCond.GraphPane
                .CurveList.Clear()
                With .AddCurve("", Me.vxLiqThCond.ToArray(GetType(Double)), Me.vyLiqThCond.ToArray(GetType(Double)), Color.Blue, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
                .Title.IsVisible = False
                .XAxis.Title.Text = "T [ " & su.temperature & " ] "
                .YAxis.Title.Text = DWSIM.App.GetLocalString("CondutividadetrmicaF2") & " [ " & su.thermalConductivity & " ] "
                .AxisChange(Me.CreateGraphics)
            End With
            Me.GraphLiqThermCond.Invalidate()


            '======== solid properties ====================================================================
            'solid density
            With constprop
                Tmin = .Solid_Density_Tmin
                Tmax = .Solid_Density_Tmax
                If Tmin = 0 Then Tmin = 50
                If Tmax = 0 Then Tmax = .TemperatureOfFusion
                If .TemperatureOfFusion = 0 Then Tmax = .Normal_Boiling_Point * 0.3
                delta = (Tmax - Tmin) / 50
            End With
            T = Tmin
            Row = 0
            vxSD.Clear()
            vySD.Clear()
            If Not constprop.IsIon And Not constprop.IsSalt Then
                Do
                    TD = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                    VD = SystemsOfUnits.Converter.ConvertFromSI(su.density, pp.AUX_SOLIDDENSi(constprop, T))
                    vxSD.Add(TD)
                    vySD.Add(VD)
                    'Me.DataTable.Item(10, Row).Value = Format(TD, nf)
                    'Me.DataTable.Item(11, Row).Value = Format(VD, nf)

                    Me.DataTableSolid.Item(0, Row).Value = Format(TD, nf)
                    Me.DataTableSolid.Item(1, Row).Value = Format(VD, nf)
                    T += delta
                    Row += 1
                Loop Until Row = 51
            End If

            With Me.GraphSolidDens.GraphPane
                .CurveList.Clear()
                With .AddCurve("", Me.vxSD.ToArray(GetType(Double)), Me.vySD.ToArray(GetType(Double)), Color.Blue, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
                .Title.IsVisible = False
                .XAxis.Title.Text = "T [ " & su.temperature & " ] "
                .YAxis.Title.Text = DWSIM.App.GetLocalString("SolidDensity") & " [ " & su.density & " ] "
                .AxisChange(Me.CreateGraphics)
            End With
            Me.GraphSolidDens.Invalidate()


            'solid heat capacity
            With constprop
                Tmin = .Solid_Heat_Capacity_Tmin
                Tmax = .Solid_Heat_Capacity_Tmax
                If Tmin = 0 Then Tmin = 50
                If Tmax = 0 Then Tmax = .TemperatureOfFusion
                If .TemperatureOfFusion = 0 Then Tmax = .Normal_Boiling_Point * 0.3
                delta = (Tmax - Tmin) / 50
            End With
            T = Tmin
            Row = 0
            vxSCP.Clear()
            vySCP.Clear()
            If Not constprop.IsIon And Not constprop.IsSalt Then
                Do
                    TD = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                    VD = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, pp.AUX_SolidHeatCapacity(constprop, T))
                    vxSCP.Add(TD)
                    vySCP.Add(VD)
                    'Me.DataTable.Item(12, Row).Value = Format(TD, nf)
                    'Me.DataTable.Item(13, Row).Value = Format(VD, nf)

                    Me.DataTableSolid.Item(2, Row).Value = Format(TD, nf)
                    Me.DataTableSolid.Item(3, Row).Value = Format(VD, nf)
                    T += delta
                    Row += 1
                Loop Until Row = 51
            End If

            With Me.GraphCpSolid.GraphPane
                .CurveList.Clear()
                With .AddCurve("", Me.vxSCP.ToArray(GetType(Double)), Me.vySCP.ToArray(GetType(Double)), Color.Blue, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
                .Title.IsVisible = False
                .XAxis.Title.Text = "T [ " & su.temperature & " ] "
                .YAxis.Title.Text = DWSIM.App.GetLocalString("SolidCp") & " [ " & su.heatCapacityCp & " ] "
                .AxisChange(Me.CreateGraphics)
            End With
            Me.GraphCpSolid.Invalidate()

            '======== finished themperature dependent properties ====================================================


            'UNIFAC
            tbUNIFAC.Text = ""
            If Not constprop.UNIFACGroups Is Nothing Then
                Dim unif As New Thermodynamics.PropertyPackages.Auxiliary.Unifac
                For Each s As String In constprop.UNIFACGroups.Keys
                    tbUNIFAC.Text += unif.ID2Group(s) & " = " & constprop.UNIFACGroups(s) & ", "
                Next
                tbUNIFAC.Text = tbUNIFAC.Text.TrimEnd(New Char() {",", " "})
            End If

            'MODFAC
            tbMODFAC.Text = ""
            If Not constprop.MODFACGroups Is Nothing Then
                Dim unif As New Thermodynamics.PropertyPackages.Auxiliary.Modfac
                For Each s As String In constprop.MODFACGroups.Keys
                    tbMODFAC.Text += unif.ID2Group(s) & " = " & constprop.MODFACGroups(s) & ", "
                Next
                tbMODFAC.Text = tbMODFAC.Text.TrimEnd(New Char() {",", " "})
            End If

            'MODFAC
            tbMODFACNIST.Text = ""
            If Not constprop.NISTMODFACGroups Is Nothing Then
                Dim unif As New Thermodynamics.PropertyPackages.Auxiliary.NISTMFAC
                For Each s As String In constprop.NISTMODFACGroups.Keys
                    tbMODFACNIST.Text += unif.ID2Group(s) & " = " & constprop.NISTMODFACGroups(s) & ", "
                Next
                tbMODFACNIST.Text = tbMODFACNIST.Text.TrimEnd(New Char() {",", " "})
            End If

            tbFormula.Text = constprop.Formula
            tbSMILES.Text = constprop.SMILES
            tbInChI.Text = constprop.InChI

            tbElements.Text = ""
            For Each el In constprop.Elements.Keys
                tbElements.Text += el + " = " + constprop.Elements(el).ToString + ", "
            Next
            tbElements.Text = tbElements.Text.TrimEnd().TrimEnd(",")

            If Not DWSIM.App.IsRunningOnMono Then

                'Render molecule / Calculate InChI from SMILES
                If Not constprop.SMILES Is Nothing And Not constprop.SMILES = "" Then

                    'definition available, render molecule
                    Try
                        ind = New Indigo()
                        mol = ind.loadMolecule(constprop.SMILES)
                        renderer = New IndigoRenderer(ind)

                        If constprop.InChI = "" Then
                            Dim ii As New IndigoInchi(ind)
                            tbInChI.Text = ii.getInchi(mol)
                        End If

                        With renderer
                            ind.setOption("render-image-size", pbRender.Width, pbRender.Height)
                            ind.setOption("render-margins", 15, 15)
                            ind.setOption("render-coloring", True)
                            ind.setOption("render-background-color", Color.White)
                        End With

                        pbRender.Image = renderer.renderToBitmap(mol)

                    Catch ex As Exception

                        MessageBox.Show(ex.ToString, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)

                    End Try

                Else

                    'no definition available, delete old picture
                    pbRender.Image = Nothing

                End If

            Else

                'no definition available, delete old picture
                pbRender.Image = Nothing

            End If

        End If

        'Property Grid
        With Me.GridProps.Rows
            .Clear()
            .Add(New Object() {DWSIM.App.GetLocalString("Database"), constprop.OriginalDB, ""})
            .Add(New Object() {DWSIM.App.GetLocalString("Type"), DWSIM.App.GetComponentType(constprop), ""})
            .Add(New Object() {"ID", constprop.ID, ""})
            .Add(New Object() {DWSIM.App.GetLocalString("CASNumber"), constprop.CAS_Number, ""})
            .Add(New Object() {DWSIM.App.GetLocalString("Solid"), constprop.IsSolid.ToString(), ""})
            .Add(New Object() {DWSIM.App.GetLocalString("Petroleum Fraction"), CBool(constprop.IsPF).ToString(), ""})
            .Add(New Object() {DWSIM.App.GetLocalString("Salt"), constprop.IsSalt.ToString(), ""})
            .Add(New Object() {DWSIM.App.GetLocalString("Hydrated Salt"), constprop.IsHydratedSalt.ToString(), ""})
            .Add(New Object() {DWSIM.App.GetLocalString("Ion"), constprop.IsIon.ToString(), ""})
            .Add(New Object() {DWSIM.App.GetLocalString("Massamolar"), Format(constprop.Molar_Weight, nf), su.molecularWeight})
            .Add(New Object() {DWSIM.App.GetLocalString("TemperaturaCrtica"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, constprop.Critical_Temperature), nf), su.temperature})
            .Add(New Object() {DWSIM.App.GetLocalString("PressoCrtica"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, constprop.Critical_Pressure), nf), su.pressure})
            .Add(New Object() {DWSIM.App.GetLocalString("VolumeCrtico"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, constprop.Critical_Volume), nf), su.molar_volume})
            .Add(New Object() {DWSIM.App.GetLocalString("CompressibilidadeCrt"), Format(constprop.Critical_Compressibility, nf), ""})
            .Add(New Object() {DWSIM.App.GetLocalString("FatorAcntrico"), Format(constprop.Acentric_Factor, nf), ""})
            .Add(New Object() {DWSIM.App.GetLocalString("EntalpiadeFormaodoGs"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, constprop.IG_Enthalpy_of_Formation_25C), nf), su.enthalpy})
            .Add(New Object() {DWSIM.App.GetLocalString("EnergyFlowdeGibbsdeForm2"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, constprop.IG_Gibbs_Energy_of_Formation_25C), nf), su.enthalpy})
            .Add(New Object() {DWSIM.App.GetLocalString("PontoNormaldeEbulio"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, constprop.Normal_Boiling_Point), nf), su.temperature})
            .Add(New Object() {DWSIM.App.GetLocalString("TemperatureOfFusion"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, constprop.TemperatureOfFusion), nf), su.temperature})
            .Add(New Object() {DWSIM.App.GetLocalString("EnthalpyOfFusionAtTf"), Format(constprop.EnthalpyOfFusionAtTf, nf), "kJ/mol"})
            .Add(New Object() {DWSIM.App.GetLocalString("TemperatureOfSolidDensity_Ts"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, constprop.SolidTs), nf), su.temperature})
            .Add(New Object() {DWSIM.App.GetLocalString("SolidDensityAtTs"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.density, constprop.SolidDensityAtTs), nf), su.density})
            .Add(New Object() {DWSIM.App.GetLocalString("ChaoSeaderAcentricFactor"), Format(constprop.Chao_Seader_Acentricity, nf), "-"})
            .Add(New Object() {DWSIM.App.GetLocalString("ChaoSeaderSolubilityParameter"), Format(constprop.Chao_Seader_Solubility_Parameter, nf), "(cal/mL)^0.5"})
            .Add(New Object() {DWSIM.App.GetLocalString("ChaoSeaderLiquidMolarVolume"), Format(constprop.Chao_Seader_Liquid_Molar_Volume, nf), "mL/mol"})
            .Add(New Object() {DWSIM.App.GetLocalString("RackettCompressibility"), Format(constprop.Z_Rackett, nf), ""})
            .Add(New Object() {DWSIM.App.GetLocalString("PengRobinsonVolumeTranslationCoefficient"), Format(constprop.PR_Volume_Translation_Coefficient, nf), ""})
            .Add(New Object() {DWSIM.App.GetLocalString("SRKVolumeTranslationCoefficient"), Format(constprop.SRK_Volume_Translation_Coefficient, nf), ""})
            .Add(New Object() {"COSTALD Characteristic Volume", Format(constprop.COSTALD_Characteristic_Volume, nf), "m3/kmol"})
            .Add(New Object() {"COSTALD SRK Acentric Factor", Format(constprop.COSTALD_SRK_Acentric_Factor, nf), ""})
            .Add(New Object() {"UNIQUAC R", Format(constprop.UNIQUAC_R, nf), ""})
            .Add(New Object() {"UNIQUAC Q", Format(constprop.UNIQUAC_Q, nf), ""})
            .Add(New Object() {DWSIM.App.GetLocalString("Charge"), Format(constprop.Charge, "#;-#"), ""})
            .Add(New Object() {DWSIM.App.GetLocalString("HydrationNumber"), constprop.HydrationNumber, ""})
            .Add(New Object() {DWSIM.App.GetLocalString("PositiveIon"), constprop.PositiveIon, ""})
            .Add(New Object() {DWSIM.App.GetLocalString("NegativeIon"), constprop.NegativeIon, ""})
            .Add(New Object() {DWSIM.App.GetLocalString("Electrolyte_DelGF"), constprop.Electrolyte_DelGF.ToString(nf), "kJ/mol"})
            .Add(New Object() {DWSIM.App.GetLocalString("Electrolyte_DelHF"), constprop.Electrolyte_DelHF.ToString(nf), "kJ/mol"})
            .Add(New Object() {DWSIM.App.GetLocalString("Electrolyte_Cp0"), Format(constprop.Electrolyte_Cp0, nf), "kJ/[mol.K]"})
            .Add(New Object() {DWSIM.App.GetLocalString("Electrolyte_StdStateMolVol"), Format(constprop.StandardStateMolarVolume, nf), "cm3/mol"})
            .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_SGG"), Format(constprop.BO_SGG, nf), ""})
            .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_SGO"), Format(constprop.BO_SGO, nf), ""})
            .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_GOR"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.gor, constprop.BO_GOR), nf), su.gor})
            .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_BSW"), Format(constprop.BO_BSW, nf), ""})
            .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_V1"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.cinematic_viscosity, constprop.BO_OilVisc1), nf), su.cinematic_viscosity})
            .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_T1"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, constprop.BO_OilViscTemp1), nf), su.temperature})
            .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_V2"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.cinematic_viscosity, constprop.BO_OilVisc2), nf), su.cinematic_viscosity})
            .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_T2"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, constprop.BO_OilViscTemp2), nf), su.temperature})
            .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_PNA_P"), Format(constprop.BO_PNA_P, nf), ""})
            .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_PNA_N"), Format(constprop.BO_PNA_N, nf), ""})
            .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_PNA_A"), Format(constprop.BO_PNA_A, nf), ""})
        End With

        TextBoxComments.Text = constprop.Comments

        chkEnableEdit_CheckedChanged(Me, New EventArgs)

    End Sub

    Private Sub GridProps_CellEndEdit(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles GridProps.CellEndEdit

        Dim cv As New SystemsOfUnits.Converter
        Dim su As SystemsOfUnits.Units = Flowsheet.Options.SelectedUnitSystem
        Select Case e.RowIndex
            Case 0 '.Add(New Object() {DWSIM.App.GetLocalString("Database"), constprop.CurrentDB, ""})
            Case 1 '.Add(New Object() {DWSIM.App.GetLocalString("Type"), DWSIM.App.GetComponentType(constprop), ""})
            Case 2 '.Add(New Object() {"ID", constprop.ID, ""})
            Case 3 '.Add(New Object() {DWSIM.App.GetLocalString("CASNumber"), constprop.CAS_Number, ""})
            Case 4 '.Add(New Object() {DWSIM.App.GetLocalString("Massamolar"), Format(constprop.Molar_Weight, nf), su.molecularWeight})
                constprop.Molar_Weight = SystemsOfUnits.Converter.ConvertToSI(su.molecularWeight, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 5 '.Add(New Object() {DWSIM.App.GetLocalString("TemperaturaCrtica"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, constprop.Critical_Temperature), nf), su.temperature})
                constprop.Critical_Temperature = SystemsOfUnits.Converter.ConvertToSI(su.temperature, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 6 '.Add(New Object() {DWSIM.App.GetLocalString("PressoCrtica"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, constprop.Critical_Pressure), nf), su.pressure})
                constprop.Critical_Pressure = SystemsOfUnits.Converter.ConvertToSI(su.pressure, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 7 '.Add(New Object() {DWSIM.App.GetLocalString("VolumeCrtico"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, constprop.Critical_Volume), nf), su.molar_volume})
                constprop.Critical_Volume = SystemsOfUnits.Converter.ConvertToSI(su.volume, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 8 '.Add(New Object() {DWSIM.App.GetLocalString("CompressibilidadeCrt"), Format(constprop.Critical_Compressibility, nf), ""})
                constprop.Critical_Compressibility = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 9 '.Add(New Object() {DWSIM.App.GetLocalString("FatorAcntrico"), Format(constprop.Acentric_Factor, nf), ""})
                constprop.Acentric_Factor = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 10 '.Add(New Object() {DWSIM.App.GetLocalString("EntalpiadeFormaodoGs"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, constprop.IG_Enthalpy_of_Formation_25C), nf), su.enthalpy})
                constprop.IG_Enthalpy_of_Formation_25C = SystemsOfUnits.Converter.ConvertToSI(su.enthalpy, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 11 '.Add(New Object() {DWSIM.App.GetLocalString("EnergyFlowdeGibbsdeForm2"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, constprop.IG_Gibbs_Energy_of_Formation_25C), nf), su.enthalpy})
                constprop.IG_Gibbs_Energy_of_Formation_25C = SystemsOfUnits.Converter.ConvertToSI(su.enthalpy, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 12 '.Add(New Object() {DWSIM.App.GetLocalString("PontoNormaldeEbulio"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, constprop.Normal_Boiling_Point), nf), su.temperature})
                constprop.Normal_Boiling_Point = SystemsOfUnits.Converter.ConvertToSI(su.temperature, GridProps.Rows(e.RowIndex).Cells(1).Value)
                constprop.NBP = SystemsOfUnits.Converter.ConvertToSI(su.temperature, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 17 '.Add(New Object() {DWSIM.App.GetLocalString("ChaoSeaderAcentricFactor"), Format(constprop.Chao_Seader_Acentricity, nf), "-"})
                constprop.Chao_Seader_Acentricity = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 18 '.Add(New Object() {DWSIM.App.GetLocalString("ChaoSeaderSolubilityParameter"), Format(constprop.Chao_Seader_Solubility_Parameter, nf), "(cal/mL)^0.5"})
                constprop.Chao_Seader_Solubility_Parameter = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 19 '.Add(New Object() {DWSIM.App.GetLocalString("ChaoSeaderLiquidMolarVolume"), Format(constprop.Chao_Seader_Liquid_Molar_Volume, nf), "mL/mol"})
                constprop.Chao_Seader_Liquid_Molar_Volume = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 20 '.Add(New Object() {DWSIM.App.GetLocalString("RackettCompressibility"), Format(constprop.Z_Rackett, nf), ""})
                constprop.Z_Rackett = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 21 '.Add(New Object() {DWSIM.App.GetLocalString("PengRobinsonVolumeTranslationCoefficient"), Format(constprop.PR_Volume_Translation_Coefficient, nf), ""})
                constprop.PR_Volume_Translation_Coefficient = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 22 '.Add(New Object() {DWSIM.App.GetLocalString("SRKVolumeTranslationCoefficient"), Format(constprop.SRK_Volume_Translation_Coefficient, nf), ""})
                constprop.SRK_Volume_Translation_Coefficient = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 23 '.Add(New Object() {DWSIM.App.GetLocalString("SRKVolumeTranslationCoefficient"), Format(constprop.SRK_Volume_Translation_Coefficient, nf), ""})
                constprop.COSTALD_Characteristic_Volume = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 24 '.Add(New Object() {DWSIM.App.GetLocalString("SRKVolumeTranslationCoefficient"), Format(constprop.SRK_Volume_Translation_Coefficient, nf), ""})
                constprop.COSTALD_SRK_Acentric_Factor = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 25 '.Add(New Object() {"UNIQUAC R", Format(constprop.UNIQUAC_R, nf), ""})
                constprop.UNIQUAC_R = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 26 '.Add(New Object() {"UNIQUAC Q", Format(constprop.UNIQUAC_Q, nf), ""})
                constprop.UNIQUAC_Q = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 27 '.Add(New Object() {DWSIM.App.GetLocalString("Charge"), Format(constprop.Charge, "+#;-#"), ""})
                constprop.Charge = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 28 '.Add(New Object() {DWSIM.App.GetLocalString("HydrationNumber"), constprop.HydrationNumber, ""})
                constprop.HydrationNumber = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 29 '.Add(New Object() {DWSIM.App.GetLocalString("PositiveIon"), constprop.PositiveIon, ""})
                constprop.PositiveIon = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 30 '.Add(New Object() {DWSIM.App.GetLocalString("NegativeIon"), constprop.NegativeIon, ""})
                constprop.NegativeIon = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 13 '.Add(New Object() {DWSIM.App.GetLocalString("TemperatureOfFusion"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, constprop.TemperatureOfFusion), nf), su.temperature})
                constprop.TemperatureOfFusion = SystemsOfUnits.Converter.ConvertToSI(su.temperature, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 14 '.Add(New Object() {DWSIM.App.GetLocalString("EnthalpyOfFusionAtTf"), Format(constprop.EnthalpyOfFusionAtTf, nf), "kJ/mol"})
                constprop.EnthalpyOfFusionAtTf = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 15 '.Add(New Object() {DWSIM.App.GetLocalString("TemperatureOfSolidDensity_Ts"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, constprop.SolidTs), nf), su.temperature})
                constprop.SolidTs = SystemsOfUnits.Converter.ConvertToSI(su.temperature, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 16 '.Add(New Object() {DWSIM.App.GetLocalString("SolidDensityAtTs"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.density, constprop.SolidDensityAtTs), nf), su.density})
                constprop.SolidDensityAtTs = SystemsOfUnits.Converter.ConvertToSI(su.density, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 31 '    .Add(New Object() {DWSIM.App.GetLocalString("Electrolyte_DelGF"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, constprop.Electrolyte_DelGF), nf), su.enthalpy})
                constprop.Electrolyte_DelGF = SystemsOfUnits.Converter.ConvertToSI(su.enthalpy, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 32 '    .Add(New Object() {DWSIM.App.GetLocalString("Electrolyte_DelHF"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, constprop.Electrolyte_DelGF), nf), su.enthalpy})
                constprop.Electrolyte_DelHF = SystemsOfUnits.Converter.ConvertToSI(su.enthalpy, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 33 '    .Add(New Object() {DWSIM.App.GetLocalString("Electrolyte_Cp0"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, constprop.Electrolyte_Cp0), nf), su.heatCapacityCp})
                constprop.Electrolyte_Cp0 = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 34 '    .Add(New Object() {DWSIM.App.GetLocalString("Electrolyte_StdStateMolVol"), Format(constprop.StandardStateMolarVolume, nf), "cm3/mol"})
                constprop.StandardStateMolarVolume = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 35 '    .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_SGG"), Format(constprop.BO_SGG, nf), ""})
                constprop.BO_SGG = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 36 '    .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_SGO"), Format(constprop.BO_SGO, nf), ""})
                constprop.BO_SGO = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 37 '    .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_GOR"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.gor, constprop.BO_GOR), nf), su.gor})
                constprop.BO_GOR = SystemsOfUnits.Converter.ConvertToSI(su.gor, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 38 '         .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_BSW"), Format(constprop.BO_BSW, nf), ""})
                constprop.BO_BSW = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 39 '         .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_V1"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.cinematic_viscosity, constprop.Electrolyte_DelGF), nf), su.cinematic_viscosity})
                constprop.BO_OilVisc1 = SystemsOfUnits.Converter.ConvertToSI(su.cinematic_viscosity, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 40 '         .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_T1"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, constprop.Electrolyte_DelGF), nf), su.temperature})
                constprop.BO_OilViscTemp1 = SystemsOfUnits.Converter.ConvertToSI(su.temperature, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 41 '         .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_V2"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.cinematic_viscosity, constprop.Electrolyte_DelGF), nf), su.cinematic_viscosity})
                constprop.BO_OilVisc2 = SystemsOfUnits.Converter.ConvertToSI(su.cinematic_viscosity, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 42 '         .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_T2"), Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, constprop.Electrolyte_DelGF), nf), su.temperature})
                constprop.BO_OilViscTemp2 = SystemsOfUnits.Converter.ConvertToSI(su.temperature, GridProps.Rows(e.RowIndex).Cells(1).Value)
            Case 43 '         .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_PNA_P"), Format(constprop.BO_PNA_P, nf), ""})
                constprop.BO_PNA_P = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 44 '         .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_PNA_N"), Format(constprop.BO_PNA_N, nf), ""})
                constprop.BO_PNA_N = GridProps.Rows(e.RowIndex).Cells(1).Value
            Case 45 '         .Add(New Object() {DWSIM.App.GetLocalString("BlackOil_PNA_A"), Format(constprop.BO_PNA_A, nf), ""})
                constprop.BO_PNA_A = GridProps.Rows(e.RowIndex).Cells(1).Value
        End Select

        constprop.IsModified = True
        SetCompStatus()

        For Each mat As Streams.MaterialStream In Me.Flowsheet.Collections.FlowsheetObjectCollection.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.MaterialStream)
            For Each p As BaseClasses.Phase In mat.Phases.Values
                For Each subst As BaseClasses.Compound In p.Compounds.Values
                    If subst.ConstantProperties.Name = constprop.Name Then
                        subst.ConstantProperties = constprop
                        Exit For
                    End If
                Next
            Next
        Next

    End Sub
    Private Sub SetCompStatus()
        If constprop.IsModified Then
            LblModified.Text = DWSIM.App.GetLocalString("DataModified")
            LblModified.BackColor = Color.Red
            LblModified.ForeColor = Color.White
        Else
            LblModified.Text = DWSIM.App.GetLocalString("DataOriginal")
            LblModified.BackColor = Color.Lime
            LblModified.ForeColor = Color.Black
        End If
        'Me.BtnRestaurar.Enabled = constprop.IsModified
    End Sub
    Private Sub chkEnableEdit_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles chkEnableEdit.CheckedChanged
        If chkEnableEdit.Checked Then
            Colorize(False)
        Else
            Colorize(True)
        End If
    End Sub

    Private Sub BtnRestaurar_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BtnRestaurar.Click
        Dim cpa() As BaseClasses.ConstantProperties

        Select Case constprop.OriginalDB
            Case "DWSIM"
                Dim filename As String = My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "data" & Path.DirectorySeparatorChar & "databases" & Path.DirectorySeparatorChar & "dwsim.xml"
                Try
                    Dim dwdb As New Databases.DWSIM
                    dwdb.Load()
                    cpa = dwdb.Transfer(constprop.Name)
                    If cpa.Length = 1 Then
                        constprop = cpa(0)
                        Me.Flowsheet.Options.SelectedComponents(constprop.Name) = constprop
                    End If
                Catch ex As Exception
                    ex.Data.Add("Reason", "Error loading component from DWSIM database")
                    Throw ex
                End Try
            Case "ChemSep"
                Try
                    Dim csdb As New Databases.ChemSep
                    csdb.Load()
                    cpa = csdb.Transfer(constprop.Name)
                    If cpa.Length = 1 Then
                        constprop = cpa(0)
                        Me.Flowsheet.Options.SelectedComponents(constprop.Name) = constprop
                    End If
                Catch ex As Exception
                    ex.Data.Add("Reason", "Error loading component from ChemSep database")
                    Throw ex
                End Try
            Case "User"
                'find database of component
                Dim componentes As ConstantProperties()
                For Each fpath As String In My.Settings.UserDatabases
                    componentes = Databases.UserDB.ReadComps(fpath)
                    For Each cp As ConstantProperties In componentes
                        If cp.Name = constprop.Name Then
                            constprop = cp
                            Me.Flowsheet.Options.SelectedComponents(constprop.Name) = constprop
                            Exit Select
                        End If
                    Next
                Next
            Case "Electrolytes"
                Dim filename As String = My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "data" & Path.DirectorySeparatorChar & "databases" & Path.DirectorySeparatorChar & "electrolyte.xml"
                Try
                    Dim edb As New Databases.Electrolyte
                    edb.Load()
                    cpa = edb.Transfer(constprop.Name)
                    If cpa.Length = 1 Then
                        constprop = cpa(0)
                        Me.Flowsheet.Options.SelectedComponents(constprop.Name) = constprop
                    End If
                Catch ex As Exception
                    ex.Data.Add("Reason", "Error loading component from Electrolytes database")
                    Throw ex
                End Try
            Case "Biodiesel"
                Dim filename As String = My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "data" & Path.DirectorySeparatorChar & "databases" & Path.DirectorySeparatorChar & "biod_db.xml"
                Try
                    Dim bddb As New Databases.Biodiesel
                    bddb.Load()
                    cpa = bddb.Transfer(constprop.Name)
                    If cpa.Length = 1 Then
                        constprop = cpa(0)
                        Me.Flowsheet.Options.SelectedComponents(constprop.Name) = constprop
                    End If
                Catch ex As Exception
                    ex.Data.Add("Reason", "Error loading component from Biodiesel database")
                    Throw ex
                End Try
        End Select

        For Each mat As Streams.MaterialStream In Me.Flowsheet.Collections.FlowsheetObjectCollection.Values.Where(Function(x) TypeOf x Is Streams.MaterialStream)
            For Each p As BaseClasses.Phase In mat.Phases.Values
                p.Compounds(constprop.Name).ConstantProperties = constprop
            Next
        Next

        constprop.IsModified = False
        SetCompStatus()

        Populate()

    End Sub

    Sub Colorize(ByVal LockCells As Boolean)

        GridProps.Columns(1).ReadOnly = LockCells

        For Each r As DataGridViewRow In GridProps.Rows
            If r.Index >= 4 Then
                If LockCells Then
                    r.Cells(1).ReadOnly = True
                    r.Cells(1).Style.ForeColor = Color.Black
                Else
                    r.Cells(1).ReadOnly = False
                    r.Cells(1).Style.ForeColor = Color.Red
                End If
            Else
                r.Cells(1).ReadOnly = True
                r.Cells(1).Style.ForeColor = Color.Black
            End If
        Next

    End Sub

    Private Sub FormPureComp_HelpRequested(sender As System.Object, hlpevent As System.Windows.Forms.HelpEventArgs) Handles MyBase.HelpRequested
        DWSIM.App.HelpRequested("UT_PureCompProps.htm")
    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ComboBox1.SelectedIndexChanged
        If Loaded Then
            Dim name As String = ComboBox1.SelectedItem.ToString
            MyCompound = Me.Flowsheet.Options.SelectedComponents(name)
            constprop = MyCompound
            SetCompStatus()
            Populate()
        End If
    End Sub

    Private Sub FormPureComp_Shown(sender As Object, e As EventArgs) Handles Me.Shown

        If MyCompound Is Nothing Then

            Added = True
            With Me.Flowsheet
                Me.ComboBox1.Items.Clear()
                For Each subst In .Options.SelectedComponents.Values
                    Me.ComboBox1.Items.Add(subst.Name)
                Next
            End With
            Me.ComboBox1.Enabled = True
            Loaded = True
            Me.ComboBox1.SelectedIndex = 0
        Else

            Me.ComboBox1.Items.Clear()
            Me.ComboBox1.Items.Add(MyCompound.Name)
            Me.ComboBox1.SelectedIndex = 0
            Me.ComboBox1.Enabled = False

            constprop = MyCompound
            SetCompStatus()

            Populate()

        End If

        chkEnableEdit.Enabled = Added

        Width += 1
        Refresh()

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON File", "*.json")})

        If handler IsNot Nothing Then
            Using stream As New IO.MemoryStream()
                Using writer As New StreamWriter(stream) With {.AutoFlush = True}
                    Try
                        Dim jsondata = Newtonsoft.Json.JsonConvert.SerializeObject(constprop, Newtonsoft.Json.Formatting.Indented)
                        writer.Write(jsondata)
                        handler.Write(stream)
                        MessageBox.Show(DWSIM.App.GetLocalString("FileSaved"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Information)
                    Catch ex As Exception
                        MessageBox.Show(DWSIM.App.GetLocalString("Erroaosalvararquivo") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End Try
                End Using
            End Using
        End If
    End Sub

    Private Sub FormPureComp_Resize(sender As Object, e As EventArgs) Handles Me.Resize
        If Not DWSIM.App.IsRunningOnMono Then
            If Not renderer Is Nothing Then
                ind.setOption("render-image-size", pbRender.Width, pbRender.Height)
                ind.setOption("render-margins", 15, 15)
                ind.setOption("render-coloring", True)
                ind.setOption("render-background-color", Color.White)
                pbRender.Image = renderer.renderToBitmap(mol)
            End If
        End If
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim comps As New List(Of ConstantProperties)
        If Me.sfdxml1.ShowDialog() = DialogResult.OK Then
            Try
                If Not File.Exists(sfdxml1.FileName) Then File.Create(sfdxml1.FileName).Close()
                Using stream = New FileStream(sfdxml1.FileName, FileMode.OpenOrCreate)
                    Databases.UserDB.AddCompounds(New BaseClasses.ConstantProperties() {constprop}, stream, True)
                End Using
                MessageBox.Show(DWSIM.App.GetLocalString("FileSaved"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Information)
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erroaosalvararquivo") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If
    End Sub
End Class