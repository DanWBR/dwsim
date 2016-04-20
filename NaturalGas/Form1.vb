'Natural Gas Properties Plugin for DWSIM
'Copyright 2010-2014 Daniel Medeiros

Imports FileHelpers
Imports DWSIM
Imports System.Windows.Forms
Imports System.Linq
Imports DWSIM.Thermodynamics.BaseClasses
Imports System.Threading.Tasks
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.SharedClasses.SystemsOfUnits
Imports DWSIM.DrawingTools.GraphicObjects
Imports DWSIM.Thermodynamics
Imports DWSIM.SharedClasses
Imports DWSIM.Interfaces.Enums.GraphicObjects

Public Class Form1

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    'flowsheet reference
    Public fsheet As DWSIM.FormFlowsheet

    'collection of component mass heating values
    Public dmc As New Dictionary(Of String, datamass)

    'collection of component volumetric heating values
    Public dvc As New Dictionary(Of String, datavol)

    Private Sub Form1_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing

        'remove SelectedObjectChanged event handler

        Dim eventhandler As DWSIM.frmSurface.ObjectSelectedEventHandler = AddressOf SelectedObjectChanged

        RemoveHandler fsheet.FormSurface.ObjectSelected, eventhandler

        'For Each f In fsheet.Collections.FlowsheetObjectCollection.Values
        '    If f.GraphicObject.ObjectType = ObjectType.FlowsheetUO Then
        '        RemoveHandler DirectCast(f, DWSIM.DWSIM.SimulationObjects.UnitOperations.Flowsheet).Fsheet.FormSurface.ObjectSelected, eventhandler
        '    End If
        'Next

        My.Settings.Save()

    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

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

        'add SelectedObjectChanged event handler

        Dim eventhandler As DWSIM.frmSurface.ObjectSelectedEventHandler = AddressOf SelectedObjectChanged

        AddHandler fsheet.FormSurface.ObjectSelected, eventhandler

        'For Each f In fsheet.Collections.FlowsheetObjectCollection.Values
        '    If f.GraphicObject.ObjectType = ObjectType.FlowsheetUO Then
        '        AddHandler DirectCast(f, DWSIM.DWSIM.SimulationObjects.UnitOperations.Flowsheet).Fsheet.FormSurface.ObjectSelected, eventhandler
        '    End If
        'Next

    End Sub

    Sub SelectedObjectChanged(ByVal sender As FormFlowsheet)

        Me.lblStream.Text = ""
        Me.lblCalcd.Text = ""
        Me.lblVapOnly.Text = ""

        Me.pg.ShowCustomProperties = True
        Me.pg.Item.Clear()
        Me.Invalidate()

        'check if we have a selected object.
        If Not fsheet.FormSurface.FlowsheetDesignSurface.SelectedObject Is Nothing Then

            'check if the selected object is a material stream.
            If fsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.ObjectType = Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream Then

                'get a reference to the material stream graphic object.
                Dim gobj As GraphicObject = fsheet.FormSurface.FlowsheetDesignSurface.SelectedObject

                Me.lblStream.Text = gobj.Tag
                If gobj.Calculated Then
                    Me.lblCalcd.Text = "Yes"
                Else
                    Me.lblCalcd.Text = "No"
                End If

                'get a reference to the material stream base class.
                Dim dobj As Streams.MaterialStream = fsheet.Collections.FlowsheetObjectCollection(gobj.Name)

                'check if the stream is vapor only.
                If dobj.Phases(2).Properties.molarfraction = 1 Then
                    Me.lblVapOnly.Text = "Yes"
                Else
                    Me.lblVapOnly.Text = "No"
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
                Dim c1, c2, c3, ic4, nc4, co2, n2 As Compound

                'molecular weight
                Dim mw As Double = dobj.Phases(0).Properties.molecularWeight

                'declare a temporary material stream so we can do calculations without messing with the simulation.
                Dim tmpms As New Streams.MaterialStream("", "")
                tmpms = dobj.Clone
                tmpms.PropertyPackage = dobj.PropertyPackage.Clone
                tmpms.PropertyPackage.CurrentMaterialStream = tmpms

                'get the current composition, check if there is water and create a new, "dry" composition
                'vx = current composition
                'vxnw = dry composition
                Dim vx(dobj.Phases(0).Compounds.Count - 1), vxnw(dobj.Phases(0).Compounds.Count - 1), vxw(dobj.Phases(0).Compounds.Count - 1) As Double
                Dim i As Integer = 0
                Dim iw As Integer = -1
                For Each c As Compound In dobj.Phases(0).Compounds.Values
                    vx(i) = c.MoleFraction
                    If c.ConstantProperties.CAS_Number = "7732-18-5" Then
                        iw = i
                    End If
                    i += 1
                Next

                If iw <> -1 Then
                    If vx(iw) <> 0.0# Then
                        'water is present
                        i = 0
                        For Each c As Compound In dobj.Phases(0).Compounds.Values
                            If i <> iw Then
                                vxnw(i) = vx(i) / (1 - vx(iw))
                                vxw(i) = 0.0#
                            Else
                                vxnw(i) = 0.0#
                                vxw(i) = 1.0#
                            End If
                            i += 1
                        Next
                    End If
                Else
                    'if there is no water, clone the current composition.
                    vxnw = vx.Clone
                End If

                Me.pg.ShowCustomProperties = True
                Me.pg.Item.Clear()
                Me.pg.Item.Add("Calculating", "please wait...", True, "Natural Gas Properties", "", True)
                Me.pg.PropertySort = PropertySort.Categorized
                Me.pg.ShowCustomProperties = True

                'wdp    =   Water dew point (real, not reliable)
                'hdp    =   Hydrocarbon dew point
                '           Calculated using the dry composition and a normal PV-Flash.
                'iwdp   =   Ideal water dew point
                '           Calculated based on the Raoult's law:
                '           xiPisat = yiP => Pisat = yiP/xi
                '           After calculating Pisat (water partial vapor pressure), use the AUX_TSATi function 
                '           to return the saturation temperature (dew point).
                Dim wdp, hdp, iwdp, wc0, wc15, wc20, wcb, wdp1, iwdp1, hdp1 As Double
                Dim fa As New DWSIM.Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.NestedLoops3PV2
                Dim fa2 As New DWSIM.Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.NestedLoops3PV2
                fa.StabSearchCompIDs = New String() {"Agua", "Water"}
                fa.StabSearchSeverity = 2

                Dim pp1 As PropertyPackage = tmpms.PropertyPackage
                Dim pp2 As PropertyPackage = tmpms.PropertyPackage.Clone

                pp1.CurrentMaterialStream = tmpms
                pp2.CurrentMaterialStream = tmpms.Clone

                Try
                    If iw <> -1 Then
                        If vx(iw) <> 0.0# Then
                            Dim t1 As Task = Task.Factory.StartNew(Sub()
                                                                       Dim result As Object = pp1.FlashBase.Flash_PV(vx, tmpms.Phases(0).Properties.pressure, 1, 250, pp1)
                                                                       iwdp = pp1.AUX_TSATi(vx(iw) * tmpms.Phases(0).Properties.pressure.GetValueOrDefault, iw)
                                                                       hdp = pp1.FlashBase.Flash_PV(vxnw, tmpms.Phases(0).Properties.pressure, 1, result(4), pp1)(4)
                                                                       wdp = fa.Flash_PV_3P(vx, 0.999, 0.0000000001, 0.001, result(3), result(2), vxw, tmpms.Phases(0).Properties.pressure, 0.999, iwdp, pp1)(4)

                                                                   End Sub)
                            Dim t2 As Task = Task.Factory.StartNew(Sub()
                                                                       Dim result As Object = pp2.FlashBase.Flash_PV(vx, 101325, 1, 250, pp2)
                                                                       iwdp1 = pp2.AUX_TSATi(vx(iw) * 101325, iw)
                                                                       hdp1 = pp2.FlashBase.Flash_PV(vxnw, 101325, 1, result(4), pp2)(4)
                                                                       wdp1 = fa2.Flash_PV_3P(vx, 0.999, 0.0000000001, 0.001, result(3), result(2), vxw, 101325, 0.999, iwdp1, pp2)(4)
                                                                   End Sub)
                            Threading.Thread.Sleep(500)
                            While t1.Status = TaskStatus.Running Or t2.Status = TaskStatus.Running
                                Threading.Thread.Sleep(500)
                                Application.DoEvents()
                            End While
                        End If
                    Else
                        wdp = -1.0E+20
                        iwdp = -1.0E+20
                    End If
                Catch ex As Exception
                    fsheet.WriteToLog(ex.ToString, Drawing.Color.Red, DWSIM.DWSIM.Flowsheet.MessageType.GeneralError)
                Finally
                    fa = Nothing
                    fa2 = Nothing
                    pp2.Dispose()
                    pp2.CurrentMaterialStream = Nothing
                    pp2 = Nothing
                End Try

                'set stream pressure
                tmpms.Phases(0).Properties.pressure = 101325

                'compressibility factors and specific gravities
                Dim z0, z15, z20, d0, d15, d20, d As Double

                'ideal gas specific gravity
                d = mw / 28.9626

                tmpms.Phases(0).Properties.temperature = 273.15 + 0
                tmpms.PropertyPackage.DW_CalcPhaseProps(DWSIM.Thermodynamics.PropertyPackages.Phase.Vapor)
                z0 = tmpms.Phases(2).Properties.compressibilityFactor
                d0 = d / z0

                tmpms.Phases(0).Properties.temperature = 273.15 + 15.56
                tmpms.PropertyPackage.DW_CalcPhaseProps(DWSIM.Thermodynamics.PropertyPackages.Phase.Vapor)
                z15 = tmpms.Phases(2).Properties.compressibilityFactor
                d15 = d / z15

                tmpms.Phases(0).Properties.temperature = 273.15 + 20
                tmpms.PropertyPackage.DW_CalcPhaseProps(DWSIM.Thermodynamics.PropertyPackages.Phase.Vapor)
                z20 = tmpms.Phases(2).Properties.compressibilityFactor
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
                        hhv25m += c.MoleFraction * c.ConstantProperties.Molar_Weight / mw * dmc(c.ConstantProperties.CAS_Number).sup25 * 1000
                        hhv20m += c.MoleFraction * c.ConstantProperties.Molar_Weight / mw * dmc(c.ConstantProperties.CAS_Number).sup20 * 1000
                        hhv15m += c.MoleFraction * c.ConstantProperties.Molar_Weight / mw * dmc(c.ConstantProperties.CAS_Number).sup15 * 1000
                        hhv0m += c.MoleFraction * c.ConstantProperties.Molar_Weight / mw * dmc(c.ConstantProperties.CAS_Number).sup0 * 1000
                        lhv25m += c.MoleFraction * c.ConstantProperties.Molar_Weight / mw * dmc(c.ConstantProperties.CAS_Number).inf25 * 1000
                        lhv20m += c.MoleFraction * c.ConstantProperties.Molar_Weight / mw * dmc(c.ConstantProperties.CAS_Number).inf20 * 1000
                        lhv15m += c.MoleFraction * c.ConstantProperties.Molar_Weight / mw * dmc(c.ConstantProperties.CAS_Number).inf15 * 1000
                        lhv0m += c.MoleFraction * c.ConstantProperties.Molar_Weight / mw * dmc(c.ConstantProperties.CAS_Number).inf0 * 1000
                    End If
                    If dvc.ContainsKey(c.ConstantProperties.CAS_Number) Then
                        hhv1515v += c.MoleFraction * dvc(c.ConstantProperties.CAS_Number).sup1515 * 1000
                        hhv00v += c.MoleFraction * dvc(c.ConstantProperties.CAS_Number).sup00 * 1000
                        hhv2020v += c.MoleFraction * dvc(c.ConstantProperties.CAS_Number).sup2020 * 1000
                        lhv1515v += c.MoleFraction * dvc(c.ConstantProperties.CAS_Number).inf1515 * 1000
                        lhv00v += c.MoleFraction * dvc(c.ConstantProperties.CAS_Number).inf00 * 1000
                        lhv2020v += c.MoleFraction * dvc(c.ConstantProperties.CAS_Number).inf2020 * 1000
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
                c1 = (From c As Compound In dobj.Phases(0).Compounds.Values Select c Where c.ConstantProperties.CAS_Number = "74-82-8").FirstOrDefault
                c2 = (From c As Compound In dobj.Phases(0).Compounds.Values Select c Where c.ConstantProperties.CAS_Number = "74-84-0").FirstOrDefault
                c3 = (From c As Compound In dobj.Phases(0).Compounds.Values Select c Where c.ConstantProperties.CAS_Number = "74-98-6").FirstOrDefault
                nc4 = (From c As Compound In dobj.Phases(0).Compounds.Values Select c Where c.ConstantProperties.CAS_Number = "106-97-8").FirstOrDefault
                ic4 = (From c As Compound In dobj.Phases(0).Compounds.Values Select c Where c.ConstantProperties.CAS_Number = "75-28-5").FirstOrDefault
                co2 = (From c As Compound In dobj.Phases(0).Compounds.Values Select c Where c.ConstantProperties.CAS_Number = "124-38-9").FirstOrDefault
                n2 = (From c As Compound In dobj.Phases(0).Compounds.Values Select c Where c.ConstantProperties.CAS_Number = "7727-37-9").FirstOrDefault

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
                Dim nf As String = fsheet.Options.NumberFormat

                'get a reference to the current unit system.
                Dim su As SystemsOfUnits.Units = fsheet.Options.SelectedUnitSystem

                'populate property grid with calculated values.

                With Me.pg

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
                    .Item.Add("Water Dew Point @ P (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, wdp), nf), True, "Natural Gas Properties", "", True)
                    .Item.Add("Water Dew Point @ 1 atm (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, wdp1), nf), True, "Natural Gas Properties", "", True)
                    .Item.Add("Water Dew Point (Ideal) @ P (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, iwdp), nf), True, "Natural Gas Properties", "Water Dew Point at System Pressure, calculated using Raoult's Law and Water's Vapor Pressure experimental curve.", True)
                    .Item.Add("Water Dew Point (Ideal) @ 1 atm (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, iwdp1), nf), True, "Natural Gas Properties", "Water Dew Point at System Pressure, calculated using Raoult's Law and Water's Vapor Pressure experimental curve.", True)
                    .Item.Add("HC Dew Point @ P (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, hdp), nf), True, "Natural Gas Properties", "Hydrocarbon Dew Point at System Pressure", True)
                    .Item.Add("HC Dew Point @ 1 atm (" & su.temperature & ")", Format(Converter.ConvertFromSI(su.temperature, hdp1), nf), True, "Natural Gas Properties", "Hydrocarbon Dew Point at System Pressure", True)
                    .Item.Add("Water Content @ NC (mg/m3)", Format(wc0, nf), True, "Natural Gas Properties", "Water concentration at NC = Normal Conditions (T = 0 °C, P = 1 atm)", True)
                    .Item.Add("Water Content @ SC (mg/m3)", Format(wc15, nf), True, "Natural Gas Properties", "Water concentration at SC = Standard Conditions (T = 15.56 °C, P = 1 atm)", True)
                    .Item.Add("Water Content @ BR (mg/m3)", Format(wc20, nf), True, "Natural Gas Properties", "Water concentration at BR = CNTP (T = 20 °C, P = 1 atm)", True)

                    .PropertySort = PropertySort.Categorized
                    .ShowCustomProperties = True

                End With

            End If

            fsheet.FormSurface.FlowsheetDesignSurface.Focus()

        End If

    End Sub

End Class
