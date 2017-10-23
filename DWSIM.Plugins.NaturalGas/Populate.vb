Imports System.Windows.Forms
Imports FileHelpers
Imports DWSIM
Imports System.Linq
Imports DWSIM.Thermodynamics.BaseClasses
Imports System.Threading.Tasks
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.SharedClasses.SystemsOfUnits
Imports DWSIM.DrawingTools.GraphicObjects
Imports DWSIM.Thermodynamics
Imports DWSIM.SharedClasses
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.Interfaces
Imports Eto.Forms
Imports DWSIM.UI.Shared

Public Class Populate

    'collection of component mass heating values
    Public dmc As New Dictionary(Of String, datamass)

    'collection of component volumetric heating values
    Public dvc As New Dictionary(Of String, datavol)

    Public Sub Populate(fsheet As DWSIM.Interfaces.IFlowsheet, myform As Object)

        Dim f_wf As Form1 = Nothing
        Dim f_eto As Eto.Forms.Form = Nothing

        If TypeOf myform Is System.Windows.Forms.Form Then
            f_wf = CType(myform, Form1)
        Else
            f_eto = CType(myform, Eto.Forms.Form)
        End If

        'read data from text files
        Dim engine As New FileHelperEngine(Of datamass)()
        Dim compsm As datamass() = engine.ReadString(My.Resources.pc_massico)
        dmc = New Dictionary(Of String, datamass)
        For Each d In compsm
            If d.dbname <> "" Then dmc.Add(d.dbname, d)
        Next
        Dim engine2 As New FileHelperEngine(Of datavol)()
        Dim compsv As datavol() = engine2.ReadString(My.Resources.pc_volumetrico)
        dvc = New Dictionary(Of String, datavol)
        For Each d In compsv
            If d.dbname <> "" Then dvc.Add(d.dbname, d)
        Next

        If fsheet.GetSelectedFlowsheetSimulationObject(Nothing) Is Nothing Then
            If f_eto IsNot Nothing Then
                Dim scrollbox As Scrollable = DirectCast(f_eto.Content, Scrollable)
                Dim container As DynamicLayout = DirectCast(scrollbox.Content, DynamicLayout)
                container.CreateAndAddLabelRow("Calculation Results")
                container.CreateAndAddLabelRow2("You need to select a Material Stream before opening this plugin.")
            Else
                fsheet.ShowMessage("Natural Gas Properties Plugin error: You need to select a Material Stream first.", IFlowsheet.MessageType.GeneralError)
            End If
            Exit Sub
        End If

        'check if the selected object is a material stream.
        If fsheet.GetSelectedFlowsheetSimulationObject(Nothing).GraphicObject.ObjectType = Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream Then

            'get a reference to the material stream graphic object.
            Dim gobj As IGraphicObject = fsheet.GetSelectedFlowsheetSimulationObject(Nothing).GraphicObject

            'get a reference to the material stream base class.
            Dim dobj As Streams.MaterialStream = CType(fsheet.SimulationObjects(gobj.Name), Streams.MaterialStream)

            If f_wf IsNot Nothing Then
                f_wf.lblStream.Text = gobj.Tag
                If gobj.Calculated Then
                    f_wf.lblCalcd.Text = "Yes"
                Else
                    f_wf.lblCalcd.Text = "No"
                End If
                'check if the stream is vapor only.
                If dobj.Phases(2).Properties.molarfraction = 1 Then
                    f_wf.lblVapOnly.Text = "Yes"
                Else
                    f_wf.lblVapOnly.Text = "No"
                End If
            End If

            'declare heating value variables.
            Dim hhv25m As Double = 0
            Dim hhv20m As Double = 0
            Dim hhv15m As Double = 0
            Dim hhv0m As Double = 0
            Dim lhv25m As Double = 0
            Dim lhv20m As Double = 0
            Dim lhv15m As Double = 0
            Dim lhv0m As Double = 0
            Dim hhv1515v As Double = 0
            Dim hhv00v As Double = 0
            Dim hhv2020v As Double = 0
            Dim lhv1515v As Double = 0
            Dim lhv00v As Double = 0
            Dim lhv2020v As Double = 0
            Dim hhv1515vr As Double = 0
            Dim hhv00vr As Double = 0
            Dim hhv2020vr As Double = 0
            Dim lhv1515vr As Double = 0
            Dim lhv00vr As Double = 0
            Dim lhv2020vr As Double = 0

            'declare wobbe index variables.
            Dim iw0 As Double = 0
            Dim iw15 As Double = 0
            Dim iw20 As Double = 0
            Dim iw0r As Double = 0
            Dim iw15r As Double = 0
            Dim iw20r As Double = 0

            'methane number variables
            Dim mon, mn, xc1, xc2, xc3, xc4, xco2, xn2 As Double
            Dim c1, c2, c3, ic4, nc4, co2, n2 As ICompound

            'molecular weight
            Dim mw As Double = dobj.Phases(0).Properties.molecularWeight.GetValueOrDefault

            Dim vx(dobj.Phases(0).Compounds.Count - 1), vxnw(dobj.Phases(0).Compounds.Count - 1), vxw(dobj.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0
            Dim iw As Integer = -1
            For Each c As Compound In dobj.Phases(0).Compounds.Values
                vx(i) = c.MoleFraction.GetValueOrDefault
                If c.ConstantProperties.CAS_Number = "7732-18-5" Then
                    iw = i
                End If
                i += 1
            Next

            'wdp    =   Water dew point (real, not reliable)
            'hdp    =   Hydrocarbon dew point
            '           Calculated using the dry composition and a normal PV-Flash.
            'iwdp   =   Ideal water dew point
            '           Calculated based on the Raoult's law:
            '           xiPisat = yiP => Pisat = yiP/xi
            '           After calculating Pisat (water partial vapor pressure), use the AUX_TSATi function 
            '           to return the saturation temperature (dew point).
            Dim wdp, hdp, iwdp, wc0, wc15, wc20, wcb, wdp1, iwdp1, hdp1 As Double

            Dim dewpcalc = New DewPointFinder()

            Try
                dobj.PropertyPackage.CurrentMaterialStream = dobj
                Dim res1 = dewpcalc.CalcDewPoints(vx, dobj.Phases(0).Properties.pressure.GetValueOrDefault, dobj.PropertyPackage)
                If iw <> -1 Then
                    wdp = res1("W")
                    iwdp = res1("IW")
                End If
                hdp = res1("H")
            Catch ex As Exception
                fsheet.ShowMessage(ex.ToString, IFlowsheet.MessageType.GeneralError)
            End Try

            Try
                dobj.PropertyPackage.CurrentMaterialStream = dobj
                Dim res1 = dewpcalc.CalcDewPoints(vx, 101325, dobj.PropertyPackage)
                If iw <> -1 Then
                    wdp1 = res1("W")
                    iwdp1 = res1("IW")
                End If
                hdp1 = res1("H")
            Catch ex As Exception
                fsheet.ShowMessage(ex.ToString, IFlowsheet.MessageType.GeneralError)
            End Try

            'declare a temporary material stream so we can do calculations without messing with the simulation.
            Dim tmpms As New Streams.MaterialStream("", "")
            tmpms = CType(dobj.Clone, Streams.MaterialStream)
            tmpms.PropertyPackage = dobj.PropertyPackage
            tmpms.PropertyPackage.CurrentMaterialStream = tmpms

            'set stream pressure
            tmpms.Phases(0).Properties.pressure = 101325

            'compressibility factors and specific gravities
            Dim z0, z15, z20, d0, d15, d20, d As Double

            'ideal gas specific gravity
            d = mw / 28.9626

            tmpms.Phases(0).Properties.temperature = 273.15 + 0
            tmpms.PropertyPackage.DW_CalcPhaseProps(DWSIM.Thermodynamics.PropertyPackages.Phase.Vapor)
            z0 = tmpms.Phases(2).Properties.compressibilityFactor.GetValueOrDefault
            d0 = d / z0

            tmpms.Phases(0).Properties.temperature = 273.15 + 15.56
            tmpms.PropertyPackage.DW_CalcPhaseProps(DWSIM.Thermodynamics.PropertyPackages.Phase.Vapor)
            z15 = tmpms.Phases(2).Properties.compressibilityFactor.GetValueOrDefault
            d15 = d / z15

            tmpms.Phases(0).Properties.temperature = 273.15 + 20
            tmpms.PropertyPackage.DW_CalcPhaseProps(DWSIM.Thermodynamics.PropertyPackages.Phase.Vapor)
            z20 = tmpms.Phases(2).Properties.compressibilityFactor.GetValueOrDefault
            d20 = d / z20

            If iw <> -1 Then
                If vx(iw) <> 0.0# Then
                    'water content in mg/m3
                    If tmpms.Phases(0).Compounds.ContainsKey("Agua") Then
                        wcb = vx(iw) * tmpms.Phases(0).Compounds("Agua").ConstantProperties.Molar_Weight * 1000 * 1000
                    ElseIf tmpms.Phases(0).Compounds.ContainsKey("Water") Then
                        wcb = vx(iw) * tmpms.Phases(0).Compounds("Water").ConstantProperties.Molar_Weight * 1000 * 1000
                    End If
                    wc0 = wcb / (1 * z0 * 8314.47 * (273.15 + 0) / 101325)
                    wc15 = wcb / (1 * z15 * 8314.47 * (273.15 + 15.56) / 101325)
                    wc20 = wcb / (1 * z20 * 8314.47 * (273.15 + 20) / 101325)
                End If
            End If

            'calculation of heating values at various conditions
            For Each c As Compound In dobj.Phases(0).Compounds.Values
                If dmc.ContainsKey(c.ConstantProperties.CAS_Number) Then
                    hhv25m += c.MoleFraction.GetValueOrDefault * c.ConstantProperties.Molar_Weight / mw * dmc(c.ConstantProperties.CAS_Number).sup25 * 1000
                    hhv20m += c.MoleFraction.GetValueOrDefault * c.ConstantProperties.Molar_Weight / mw * dmc(c.ConstantProperties.CAS_Number).sup20 * 1000
                    hhv15m += c.MoleFraction.GetValueOrDefault * c.ConstantProperties.Molar_Weight / mw * dmc(c.ConstantProperties.CAS_Number).sup15 * 1000
                    hhv0m += c.MoleFraction.GetValueOrDefault * c.ConstantProperties.Molar_Weight / mw * dmc(c.ConstantProperties.CAS_Number).sup0 * 1000
                    lhv25m += c.MoleFraction.GetValueOrDefault * c.ConstantProperties.Molar_Weight / mw * dmc(c.ConstantProperties.CAS_Number).inf25 * 1000
                    lhv20m += c.MoleFraction.GetValueOrDefault * c.ConstantProperties.Molar_Weight / mw * dmc(c.ConstantProperties.CAS_Number).inf20 * 1000
                    lhv15m += c.MoleFraction.GetValueOrDefault * c.ConstantProperties.Molar_Weight / mw * dmc(c.ConstantProperties.CAS_Number).inf15 * 1000
                    lhv0m += c.MoleFraction.GetValueOrDefault * c.ConstantProperties.Molar_Weight / mw * dmc(c.ConstantProperties.CAS_Number).inf0 * 1000
                End If
                If dvc.ContainsKey(c.ConstantProperties.CAS_Number) Then
                    hhv1515v += c.MoleFraction.GetValueOrDefault * dvc(c.ConstantProperties.CAS_Number).sup1515 * 1000
                    hhv00v += c.MoleFraction.GetValueOrDefault * dvc(c.ConstantProperties.CAS_Number).sup00 * 1000
                    hhv2020v += c.MoleFraction.GetValueOrDefault * dvc(c.ConstantProperties.CAS_Number).sup2020 * 1000
                    lhv1515v += c.MoleFraction.GetValueOrDefault * dvc(c.ConstantProperties.CAS_Number).inf1515 * 1000
                    lhv00v += c.MoleFraction.GetValueOrDefault * dvc(c.ConstantProperties.CAS_Number).inf00 * 1000
                    lhv2020v += c.MoleFraction.GetValueOrDefault * dvc(c.ConstantProperties.CAS_Number).inf2020 * 1000
                End If
            Next

            'real gas heating values
            hhv1515vr = hhv1515v / z15
            hhv00vr = hhv00v / z0
            hhv2020vr = hhv2020v / z20
            lhv1515vr = lhv1515v / z15
            lhv00vr = lhv00v / z0
            lhv2020vr = lhv2020v / z20

            'ideal gas wobbe indexes
            iw0 = hhv00v / d ^ 0.5
            iw15 = hhv1515v / d ^ 0.5
            iw20 = hhv2020v / d ^ 0.5

            'real gas wobbe indexes
            iw0r = hhv00vr / d0 ^ 0.5
            iw15r = hhv1515vr / d15 ^ 0.5
            iw20r = hhv2020vr / d20 ^ 0.5

            'methane number
            c1 = (From c As ICompound In dobj.Phases(0).Compounds.Values Select c Where c.ConstantProperties.CAS_Number = "74-82-8").FirstOrDefault
            c2 = (From c As ICompound In dobj.Phases(0).Compounds.Values Select c Where c.ConstantProperties.CAS_Number = "74-84-0").FirstOrDefault
            c3 = (From c As ICompound In dobj.Phases(0).Compounds.Values Select c Where c.ConstantProperties.CAS_Number = "74-98-6").FirstOrDefault
            nc4 = (From c As ICompound In dobj.Phases(0).Compounds.Values Select c Where c.ConstantProperties.CAS_Number = "106-97-8").FirstOrDefault
            ic4 = (From c As ICompound In dobj.Phases(0).Compounds.Values Select c Where c.ConstantProperties.CAS_Number = "75-28-5").FirstOrDefault
            co2 = (From c As ICompound In dobj.Phases(0).Compounds.Values Select c Where c.ConstantProperties.CAS_Number = "124-38-9").FirstOrDefault
            n2 = (From c As ICompound In dobj.Phases(0).Compounds.Values Select c Where c.ConstantProperties.CAS_Number = "7727-37-9").FirstOrDefault

            If Not c1 Is Nothing Then xc1 = c1.MoleFraction.GetValueOrDefault
            If Not c2 Is Nothing Then xc2 = c2.MoleFraction.GetValueOrDefault
            If Not c3 Is Nothing Then xc3 = c3.MoleFraction.GetValueOrDefault
            If Not nc4 Is Nothing Then xc4 = nc4.MoleFraction.GetValueOrDefault
            If Not ic4 Is Nothing Then xc4 += ic4.MoleFraction.GetValueOrDefault
            If Not co2 Is Nothing Then xco2 = co2.MoleFraction.GetValueOrDefault
            If Not n2 Is Nothing Then xn2 = n2.MoleFraction.GetValueOrDefault

            mon = 137.78 * xc1 + 29.948 * xc2 - 18.193 * xc3 - 167.062 * xc4 + 181.233 * xco2 + 26.994 * xn2
            mn = 1.445 * mon - 103.42

            'get a reference to the current number format.
            Dim nf As String = fsheet.FlowsheetOptions.NumberFormat

            'get a reference to the current unit system.
            Dim su As SystemsOfUnits.Units = CType(fsheet.FlowsheetOptions.SelectedUnitSystem, Units)

            If f_wf IsNot Nothing Then

                'populate property grid with calculated values.

                With f_wf.pg

                    .Item.Clear()
                    .Item.Add("Molar Weight", Format(mw, nf), True, "Natural Gas Properties", "", True)
                    .Item.Add("Ideal Gas Specific Gravity", Format(d, nf), True, "Natural Gas Properties", "", True)
                    .Item.Add("Compressibility Factor @ NC", Format(z0, nf), True, "Natural Gas Properties", "NC = Normal Conditions (T = 0 °C, P = 1 atm)", True)
                    .Item.Add("Compressibility Factor @ SC", Format(z15, nf), True, "Natural Gas Properties", "SC = Standard Conditions (T = 15.56 °C, P = 1 atm)", True)
                    .Item.Add("Compressibility Factor @ BR", Format(z20, nf), True, "Natural Gas Properties", "BR = CNTP (T = 20 °C, P = 1 atm)", True)
                    .Item.Add("Specific Gravity @ NC", Format(d0, nf), True, "Natural Gas Properties", "NC = Normal Conditions (T = 0 °C, P = 1 atm)", True)
                    .Item.Add("Specific Gravity @ SC", Format(d15, nf), True, "Natural Gas Properties", "SC = Standard Conditions (T = 15.56 °C, P = 1 atm)", True)
                    .Item.Add("Specific Gravity @ BR", Format(d20, nf), True, "Natural Gas Properties", "BR = CNTP (T = 20 °C, P = 1 atm)", True)
                    .Item.Add("Mass LHV @ 0 °C (" & su.enthalpy & ")", Format(Converter.ConvertFromSI(su.enthalpy, lhv0m), nf), True, "Natural Gas Properties", "LHV = Lower Heating Value", True)
                    .Item.Add("Mass LHV @ 15 °C (" & su.enthalpy & ")", Format(Converter.ConvertFromSI(su.enthalpy, lhv15m), nf), True, "Natural Gas Properties", "LHV = Lower Heating Value", True)
                    .Item.Add("Mass LHV @ 20 °C (" & su.enthalpy & ")", Format(Converter.ConvertFromSI(su.enthalpy, lhv20m), nf), True, "Natural Gas Properties", "LHV = Lower Heating Value", True)
                    .Item.Add("Mass LHV @ 25 °C (" & su.enthalpy & ")", Format(Converter.ConvertFromSI(su.enthalpy, lhv25m), nf), True, "Natural Gas Properties", "LHV = Lower Heating Value", True)
                    .Item.Add("Mass HHV @ 0 °C (" & su.enthalpy & ")", Format(Converter.ConvertFromSI(su.enthalpy, hhv0m), nf), True, "Natural Gas Properties", "HHV = Higher Heating Value", True)
                    .Item.Add("Mass HHV @ 15 °C (" & su.enthalpy & ")", Format(Converter.ConvertFromSI(su.enthalpy, hhv15m), nf), True, "Natural Gas Properties", "HHV = Higher Heating Value", True)
                    .Item.Add("Mass HHV @ 20 °C (" & su.enthalpy & ")", Format(Converter.ConvertFromSI(su.enthalpy, hhv20m), nf), True, "Natural Gas Properties", "HHV = Higher Heating Value", True)
                    .Item.Add("Mass HHV @ 25 °C (" & su.enthalpy & ")", Format(Converter.ConvertFromSI(su.enthalpy, hhv25m), nf), True, "Natural Gas Properties", "HHV = Higher Heating Value", True)
                    .Item.Add("Molar LHV @ 0 °C (" & su.molar_enthalpy & ")", Format(Converter.ConvertFromSI(su.molar_enthalpy, lhv0m * mw), nf), True, "Natural Gas Properties", "LHV = Lower Heating Value", True)
                    .Item.Add("Molar LHV @ 15 °C (" & su.molar_enthalpy & ")", Format(Converter.ConvertFromSI(su.molar_enthalpy, lhv15m * mw), nf), True, "Natural Gas Properties", "LHV = Lower Heating Value", True)
                    .Item.Add("Molar LHV @ 20 °C (" & su.molar_enthalpy & ")", Format(Converter.ConvertFromSI(su.molar_enthalpy, lhv20m * mw), nf), True, "Natural Gas Properties", "LHV = Lower Heating Value", True)
                    .Item.Add("Molar LHV @ 25 °C (" & su.molar_enthalpy & ")", Format(Converter.ConvertFromSI(su.molar_enthalpy, lhv25m * mw), nf), True, "Natural Gas Properties", "LHV = Lower Heating Value", True)
                    .Item.Add("Molar HHV @ 0 °C (" & su.molar_enthalpy & ")", Format(Converter.ConvertFromSI(su.molar_enthalpy, hhv0m * mw), nf), True, "Natural Gas Properties", "HHV = Higher Heating Value", True)
                    .Item.Add("Molar HHV @ 15 °C (" & su.molar_enthalpy & ")", Format(Converter.ConvertFromSI(su.molar_enthalpy, hhv15m * mw), nf), True, "Natural Gas Properties", "HHV = Higher Heating Value", True)
                    .Item.Add("Molar HHV @ 20 °C (" & su.molar_enthalpy & ")", Format(Converter.ConvertFromSI(su.molar_enthalpy, hhv20m * mw), nf), True, "Natural Gas Properties", "HHV = Higher Heating Value", True)
                    .Item.Add("Molar HHV @ 25 °C (" & su.molar_enthalpy & ")", Format(Converter.ConvertFromSI(su.molar_enthalpy, hhv25m * mw), nf), True, "Natural Gas Properties", "HHV = Higher Heating Value", True)
                    .Item.Add("Ideal Gas Vol. LHV @ NC (kJ/m3)", Format(lhv00v, nf), True, "Natural Gas Properties", "NC = Normal Conditions (T = 0 °C, P = 1 atm)", True)
                    .Item.Add("Ideal Gas Vol. LHV @ SC (kJ/m3)", Format(lhv1515v, nf), True, "Natural Gas Properties", "SC = Standard Conditions (T = 15.56 °C, P = 1 atm)", True)
                    .Item.Add("Ideal Gas Vol. LHV @ BR (kJ/m3)", Format(lhv2020v, nf), True, "Natural Gas Properties", "BR = CNTP (T = 20 °C, P = 1 atm)", True)
                    .Item.Add("Ideal Gas Vol. HHV @ NC (kJ/m3)", Format(hhv00v, nf), True, "Natural Gas Properties", "NC = Normal Conditions (T = 0 °C, P = 1 atm)", True)
                    .Item.Add("Ideal Gas Vol. HHV @ SC (kJ/m3)", Format(hhv1515v, nf), True, "Natural Gas Properties", "SC = Standard Conditions (T = 15.56 °C, P = 1 atm)", True)
                    .Item.Add("Ideal Gas Vol. HHV @ BR (kJ/m3)", Format(hhv2020v, nf), True, "Natural Gas Properties", "BR = CNTP (T = 20 °C, P = 1 atm)", True)
                    .Item.Add("Vol. LHV @ NC (kJ/m3)", Format(lhv00vr, nf), True, "Natural Gas Properties", "NC = Normal Conditions (T = 0 °C, P = 1 atm)", True)
                    .Item.Add("Vol. LHV @ SC (kJ/m3)", Format(lhv1515vr, nf), True, "Natural Gas Properties", "SC = Standard Conditions (T = 15.56 °C, P = 1 atm)", True)
                    .Item.Add("Vol. LHV @ BR (kJ/m3)", Format(lhv2020vr, nf), True, "Natural Gas Properties", "BR = CNTP (T = 20 °C, P = 1 atm)", True)
                    .Item.Add("Vol. HHV @ NC (kJ/m3)", Format(hhv00vr, nf), True, "Natural Gas Properties", "NC = Normal Conditions (T = 0 °C, P = 1 atm)", True)
                    .Item.Add("Vol. HHV @ SC (kJ/m3)", Format(hhv1515vr, nf), True, "Natural Gas Properties", "SC = Standard Conditions (T = 15.56 °C, P = 1 atm)", True)
                    .Item.Add("Vol. HHV @ BR (kJ/m3)", Format(hhv2020vr, nf), True, "Natural Gas Properties", "BR = CNTP (T = 20 °C, P = 1 atm)", True)
                    .Item.Add("Ideal Gas Wobbe Index @ NC (kJ/m3)", Format(iw0, nf), True, "Natural Gas Properties", "NC = Normal Conditions (T = 0 °C, P = 1 atm)", True)
                    .Item.Add("Ideal Gas Wobbe Index @ SC (kJ/m3)", Format(iw15, nf), True, "Natural Gas Properties", "SC = Standard Conditions (T = 15.56 °C, P = 1 atm)", True)
                    .Item.Add("Ideal Gas Wobbe Index @ BR (kJ/m3)", Format(iw20, nf), True, "Natural Gas Properties", "BR = CNTP (T = 20 °C, P = 1 atm)", True)
                    .Item.Add("Wobbe Index @ NC (kJ/m3)", Format(iw0r, nf), True, "Natural Gas Properties", "NC = Normal Conditions (T = 0 °C, P = 1 atm)", True)
                    .Item.Add("Wobbe Index @ SC (kJ/m3)", Format(iw15r, nf), True, "Natural Gas Properties", "SC = Standard Conditions (T = 15.56 °C, P = 1 atm)", True)
                    .Item.Add("Wobbe Index @ BR (kJ/m3)", Format(iw20r, nf), True, "Natural Gas Properties", "BR = CNTP (T = 20 °C, P = 1 atm)", True)
                    .Item.Add("Motor Octane Number (MON)", Format(mon, nf), True, "Natural Gas Properties", "Motor Octane Number", True)
                    .Item.Add("Methane Number (MN)", Format(mn, nf), True, "Natural Gas Properties", "Methane Number", True)
                    .Item.Add("HC Dew Point @ P (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, hdp), nf), True, "Natural Gas Properties", "Hydrocarbon Dew Point at System Pressure", True)
                    .Item.Add("Water Dew Point @ P (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, wdp), nf), True, "Natural Gas Properties", "", True)
                    .Item.Add("Water Dew Point (Ideal) @ P (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, iwdp), nf), True, "Natural Gas Properties", "Water Dew Point at System Pressure, calculated using Raoult's Law and Water's Vapor Pressure experimental curve.", True)
                    .Item.Add("HC Dew Point @ 1 atm (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, hdp1), nf), True, "Natural Gas Properties", "Hydrocarbon Dew Point at System Pressure", True)
                    .Item.Add("Water Dew Point @ 1 atm (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, wdp1), nf), True, "Natural Gas Properties", "", True)
                    .Item.Add("Water Dew Point (Ideal) @ 1 atm (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, iwdp1), nf), True, "Natural Gas Properties", "Water Dew Point at System Pressure, calculated using Raoult's Law and Water's Vapor Pressure experimental curve.", True)
                    .Item.Add("Water Content @ NC (mg/m3)", Format(wc0, nf), True, "Natural Gas Properties", "Water concentration at NC = Normal Conditions (T = 0 °C, P = 1 atm)", True)
                    .Item.Add("Water Content @ SC (mg/m3)", Format(wc15, nf), True, "Natural Gas Properties", "Water concentration at SC = Standard Conditions (T = 15.56 °C, P = 1 atm)", True)
                    .Item.Add("Water Content @ BR (mg/m3)", Format(wc20, nf), True, "Natural Gas Properties", "Water concentration at BR = CNTP (T = 20 °C, P = 1 atm)", True)

                    .PropertySort = PropertySort.Categorized
                    .ShowCustomProperties = True

                End With

            Else

                Dim scrollbox As Scrollable = DirectCast(f_eto.Content, Scrollable)
                Dim container As DynamicLayout = DirectCast(scrollbox.Content, DynamicLayout)

                container.CreateAndAddLabelRow("Calculation Results")
                container.CreateAndAddTwoLabelsRow2("Selected Stream", gobj.Tag)
                container.CreateAndAddEmptySpace()
                container.CreateAndAddTwoLabelsRow("Molar Weight", Format(mw, nf))
                container.CreateAndAddTwoLabelsRow("Ideal Gas Specific Gravity", Format(d, nf))
                container.CreateAndAddEmptySpace()
                container.CreateAndAddTwoLabelsRow("Compressibility Factor @ NC", Format(z0, nf))
                container.CreateAndAddTwoLabelsRow("Compressibility Factor @ SC", Format(z15, nf))
                container.CreateAndAddTwoLabelsRow("Compressibility Factor @ BR", Format(z20, nf))
                container.CreateAndAddEmptySpace()
                container.CreateAndAddTwoLabelsRow("Specific Gravity @ NC", Format(d0, nf))
                container.CreateAndAddTwoLabelsRow("Specific Gravity @ SC", Format(d15, nf))
                container.CreateAndAddTwoLabelsRow("Specific Gravity @ BR", Format(d20, nf))
                container.CreateAndAddEmptySpace()
                container.CreateAndAddTwoLabelsRow("Mass LHV @ 0 °C (" & su.enthalpy & ")", Format(Converter.ConvertFromSI(su.enthalpy, lhv0m), nf))
                container.CreateAndAddTwoLabelsRow("Mass LHV @ 15 °C (" & su.enthalpy & ")", Format(Converter.ConvertFromSI(su.enthalpy, lhv15m), nf))
                container.CreateAndAddTwoLabelsRow("Mass LHV @ 20 °C (" & su.enthalpy & ")", Format(Converter.ConvertFromSI(su.enthalpy, lhv20m), nf))
                container.CreateAndAddTwoLabelsRow("Mass LHV @ 25 °C (" & su.enthalpy & ")", Format(Converter.ConvertFromSI(su.enthalpy, lhv25m), nf))
                container.CreateAndAddTwoLabelsRow("Mass HHV @ 0 °C (" & su.enthalpy & ")", Format(Converter.ConvertFromSI(su.enthalpy, hhv0m), nf))
                container.CreateAndAddTwoLabelsRow("Mass HHV @ 15 °C (" & su.enthalpy & ")", Format(Converter.ConvertFromSI(su.enthalpy, hhv15m), nf))
                container.CreateAndAddTwoLabelsRow("Mass HHV @ 20 °C (" & su.enthalpy & ")", Format(Converter.ConvertFromSI(su.enthalpy, hhv20m), nf))
                container.CreateAndAddTwoLabelsRow("Mass HHV @ 25 °C (" & su.enthalpy & ")", Format(Converter.ConvertFromSI(su.enthalpy, hhv25m), nf))
                container.CreateAndAddTwoLabelsRow("Molar LHV @ 0 °C (" & su.molar_enthalpy & ")", Format(Converter.ConvertFromSI(su.molar_enthalpy, lhv0m * mw), nf))
                container.CreateAndAddTwoLabelsRow("Molar LHV @ 15 °C (" & su.molar_enthalpy & ")", Format(Converter.ConvertFromSI(su.molar_enthalpy, lhv15m * mw), nf))
                container.CreateAndAddTwoLabelsRow("Molar LHV @ 20 °C (" & su.molar_enthalpy & ")", Format(Converter.ConvertFromSI(su.molar_enthalpy, lhv20m * mw), nf))
                container.CreateAndAddTwoLabelsRow("Molar LHV @ 25 °C (" & su.molar_enthalpy & ")", Format(Converter.ConvertFromSI(su.molar_enthalpy, lhv25m * mw), nf))
                container.CreateAndAddTwoLabelsRow("Molar HHV @ 0 °C (" & su.molar_enthalpy & ")", Format(Converter.ConvertFromSI(su.molar_enthalpy, hhv0m * mw), nf))
                container.CreateAndAddTwoLabelsRow("Molar HHV @ 15 °C (" & su.molar_enthalpy & ")", Format(Converter.ConvertFromSI(su.molar_enthalpy, hhv15m * mw), nf))
                container.CreateAndAddTwoLabelsRow("Molar HHV @ 20 °C (" & su.molar_enthalpy & ")", Format(Converter.ConvertFromSI(su.molar_enthalpy, hhv20m * mw), nf))
                container.CreateAndAddTwoLabelsRow("Molar HHV @ 25 °C (" & su.molar_enthalpy & ")", Format(Converter.ConvertFromSI(su.molar_enthalpy, hhv25m * mw), nf))
                container.CreateAndAddTwoLabelsRow("Ideal Gas Vol. LHV @ NC (kJ/m3)", Format(lhv00v, nf))
                container.CreateAndAddTwoLabelsRow("Ideal Gas Vol. LHV @ SC (kJ/m3)", Format(lhv1515v, nf))
                container.CreateAndAddTwoLabelsRow("Ideal Gas Vol. LHV @ BR (kJ/m3)", Format(lhv2020v, nf))
                container.CreateAndAddTwoLabelsRow("Ideal Gas Vol. HHV @ NC (kJ/m3)", Format(hhv00v, nf))
                container.CreateAndAddTwoLabelsRow("Ideal Gas Vol. HHV @ SC (kJ/m3)", Format(hhv1515v, nf))
                container.CreateAndAddTwoLabelsRow("Ideal Gas Vol. HHV @ BR (kJ/m3)", Format(hhv2020v, nf))
                container.CreateAndAddTwoLabelsRow("Vol. LHV @ NC (kJ/m3)", Format(lhv00vr, nf))
                container.CreateAndAddTwoLabelsRow("Vol. LHV @ SC (kJ/m3)", Format(lhv1515vr, nf))
                container.CreateAndAddTwoLabelsRow("Vol. LHV @ BR (kJ/m3)", Format(lhv2020vr, nf))
                container.CreateAndAddTwoLabelsRow("Vol. HHV @ NC (kJ/m3)", Format(hhv00vr, nf))
                container.CreateAndAddTwoLabelsRow("Vol. HHV @ SC (kJ/m3)", Format(hhv1515vr, nf))
                container.CreateAndAddTwoLabelsRow("Vol. HHV @ BR (kJ/m3)", Format(hhv2020vr, nf))
                container.CreateAndAddEmptySpace()
                container.CreateAndAddTwoLabelsRow("Ideal Gas Wobbe Index @ NC (kJ/m3)", Format(iw0, nf))
                container.CreateAndAddTwoLabelsRow("Ideal Gas Wobbe Index @ SC (kJ/m3)", Format(iw15, nf))
                container.CreateAndAddTwoLabelsRow("Ideal Gas Wobbe Index @ BR (kJ/m3)", Format(iw20, nf))
                container.CreateAndAddTwoLabelsRow("Wobbe Index @ NC (kJ/m3)", Format(iw0r, nf))
                container.CreateAndAddTwoLabelsRow("Wobbe Index @ SC (kJ/m3)", Format(iw15r, nf))
                container.CreateAndAddTwoLabelsRow("Wobbe Index @ BR (kJ/m3)", Format(iw20r, nf))
                container.CreateAndAddEmptySpace()
                container.CreateAndAddTwoLabelsRow("Motor Octane Number (MON)", Format(mon, nf))
                container.CreateAndAddTwoLabelsRow("Methane Number (MN)", Format(mn, nf))
                container.CreateAndAddEmptySpace()
                container.CreateAndAddTwoLabelsRow("HC Dew Point @ P (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, hdp), nf))
                container.CreateAndAddTwoLabelsRow("Water Dew Point @ P (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, wdp), nf))
                container.CreateAndAddTwoLabelsRow("Water Dew Point (Ideal) @ P (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, iwdp), nf))
                container.CreateAndAddTwoLabelsRow("HC Dew Point @ 1 atm (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, hdp1), nf))
                container.CreateAndAddTwoLabelsRow("Water Dew Point @ 1 atm (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, wdp1), nf))
                container.CreateAndAddTwoLabelsRow("Water Dew Point (Ideal) @ 1 atm (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, iwdp1), nf))
                container.CreateAndAddEmptySpace()
                container.CreateAndAddTwoLabelsRow("Water Content @ NC (mg/m3)", Format(wc0, nf))
                container.CreateAndAddTwoLabelsRow("Water Content @ SC (mg/m3)", Format(wc15, nf))
                container.CreateAndAddTwoLabelsRow("Water Content @ BR (mg/m3)", Format(wc20, nf))
                container.CreateAndAddEmptySpace()
                container.CreateAndAddEmptySpace()
                container.CreateAndAddDescriptionRow("* SC = Standard Conditions (T = 15.56 °C, P = 1 atm)")
                container.CreateAndAddDescriptionRow("* BR = CNTP (T = 20 °C, P = 1 atm)")
                container.CreateAndAddDescriptionRow("* NC = Normal Conditions (T = 0 °C, P = 1 atm)")

            End If


        End If





    End Sub

End Class
