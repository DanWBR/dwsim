'    Petroleum Cold Flow Properties Utility
'    Copyright 2009-2014 Daniel Wagner O. de Medeiros
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
Imports System.Math
Imports DWSIM.DrawingTools
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects

Public Class FrmColdProperties

    Inherits UserControl

    Implements Interfaces.IAttachedUtility

    Dim mat As Streams.MaterialStream
    Dim frm As FormFlowsheet

    Public su As New SystemsOfUnits.Units
    Public cv As New SystemsOfUnits.Converter
    Public nf As String

    Private FlashPoint, PourPoint, CloudPoint, FreezingPoint, RVP, TVP, RefractionIndex, Huang_I, CetaneIndex, v1, v2, kv1 As Double
    Private t10ASTM, t10TBP, bt, dp As Double

    Private Sub FrmColdProperties_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        frm = AttachedTo.GetFlowsheet

        Me.ComboBox3.Items.Clear()
        Me.ComboBox3.Items.Add(AttachedTo.GraphicObject.Tag.ToString)
        Me.ComboBox3.SelectedIndex = 0
        Me.ComboBox3.Enabled = False

        Me.su = frm.Options.SelectedUnitSystem
        Me.nf = frm.Options.NumberFormat

        Me.LabelU1.Text = su.pressure
        Me.LabelU2.Text = su.pressure
        Me.LabelU3.Text = su.viscosity
        Me.LabelU4.Text = su.viscosity
        Me.LabelU5.Text = su.temperature
        Me.LabelU6.Text = su.temperature
        Me.LabelU7.Text = su.temperature
        Me.LabelU8.Text = su.temperature

    End Sub


    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

        If Not Me.ComboBox3.SelectedItem Is Nothing Then

            Update1()

            Me.TextBox1.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, TVP), nf)
            Me.TextBox2.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, RVP), nf)
            Me.TextBox3.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.viscosity, v1), nf)
            Me.TextBox4.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.viscosity, v2), nf)
            Me.TextBox5.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, FlashPoint), nf)
            Me.TextBox6.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, PourPoint), nf)
            Me.TextBox7.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, CloudPoint), nf)
            Me.TextBox8.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, FreezingPoint), nf)
            Me.TextBox9.Text = Format(RefractionIndex, nf)
            Me.TextBox10.Text = Format(CetaneIndex, nf)

        End If

    End Sub

    Public Property AttachedTo As Interfaces.ISimulationObject Implements Interfaces.IAttachedUtility.AttachedTo

    Public Function GetPropertyList() As List(Of String) Implements Interfaces.IAttachedUtility.GetPropertyList
        Return New List(Of String)({"Flash Point", "Pour Point", "Cloud Point", "Freezing Point", "Reid Vapor Pressure @ 100 F", "True Vapor Pressure @ 100 F", "Refraction Index @ 20 C", "Cetane Index", "Viscosity @ 100 F", "Viscosity @ 210 F", "T 10% ASTM D86", "T 10% TBP"})
    End Function

    Public Function GetPropertyUnits(pname As String) As String Implements Interfaces.IAttachedUtility.GetPropertyUnits

        Dim su = AttachedTo.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem

        Select Case pname
            Case "Flash Point"
                Return su.temperature
            Case "Pour Point"
                Return su.temperature
            Case "Cloud Point"
                Return su.temperature
            Case "Freezing Point"
                Return su.temperature
            Case "Reid Vapor Pressure @ 100 F"
                Return su.pressure
            Case "True Vapor Pressure @ 100 F"
                Return su.pressure
            Case "Refraction Index @ 20 C"
                Return ""
            Case "Cetane Index"
                Return ""
            Case "Viscosity @ 100 F"
                Return su.viscosity
            Case "Viscosity @ 210 F"
                Return su.viscosity
            Case "T 10% ASTM D86"
                Return su.temperature
            Case "T 10% TBP"
                Return su.temperature
            Case Else
                Return ""
        End Select

    End Function

    Public Function GetPropertyValue(pname As String) As Object Implements Interfaces.IAttachedUtility.GetPropertyValue

        Select Case pname
            Case "Flash Point"
                Return FlashPoint.ConvertFromSI(su.temperature)
            Case "Pour Point"
                Return PourPoint.ConvertFromSI(su.temperature)
            Case "Cloud Point"
                Return CloudPoint.ConvertFromSI(su.temperature)
            Case "Freezing Point"
                Return FreezingPoint.ConvertFromSI(su.temperature)
            Case "Reid Vapor Pressure @ 100 F"
                Return RVP.ConvertFromSI(su.pressure)
            Case "True Vapor Pressure @ 100 F"
                Return TVP.ConvertFromSI(su.pressure)
            Case "Refraction Index @ 20 C"
                Return RefractionIndex
            Case "Cetane Index"
                Return CetaneIndex
            Case "Viscosity @ 100 F"
                Return v1.ConvertFromSI(su.viscosity)
            Case "Viscosity @ 210 F"
                Return v2.ConvertFromSI(su.viscosity)
            Case "T 10% ASTM D86"
                Return t10ASTM.ConvertFromSI(su.temperature)
            Case "T 10% TBP"
                Return t10TBP.ConvertFromSI(su.temperature)
            Case Else
                Return ""
        End Select

    End Function

    Public Property ID As Integer Implements Interfaces.IAttachedUtility.ID

    Public Property Name1 As String Implements Interfaces.IAttachedUtility.Name

    Public Sub SetPropertyValue(pname As String, pvalue As Object) Implements Interfaces.IAttachedUtility.SetPropertyValue

    End Sub

    Public Sub Update1() Implements Interfaces.IAttachedUtility.Update

        If AttachedTo Is Nothing Then
            Dim gobj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(Me.ComboBox3.SelectedItem, frm.FormSurface.FlowsheetSurface)
            Me.mat = frm.Collections.FlowsheetObjectCollection(gobj.Name).Clone
            Me.mat.SetFlowsheet(frm.Collections.FlowsheetObjectCollection(gobj.Name).GetFlowsheet)
        Else
            Me.mat = AttachedTo.Clone
            Me.mat.SetFlowsheet(AttachedTo.GetFlowsheet)
        End If

        Dim pp As PropertyPackages.PropertyPackage = mat.PropertyPackage
        Dim MABP, CABP, MeABP, K, SG, API As Double

        pp.CurrentMaterialStream = mat

        If TypeOf pp Is PropertyPackages.BlackOilPropertyPackage Then

            Dim bopp As PropertyPackages.BlackOilPropertyPackage = DirectCast(pp, PropertyPackages.BlackOilPropertyPackage)

            Dim bof = bopp.CalcBOFluid(bopp.RET_VMOL(PropertyPackages.Phase.Mixture), bopp.DW_GetConstantProperties)

            Dim bop As New Thermodynamics.PropertyPackages.Auxiliary.BlackOilProperties

            MeABP = bop.LiquidNormalBoilingPoint(bof.SGO, bof.BSW)
            SG = bof.SGO
            K = (1.8 * MeABP) ^ (1 / 3) / SG
            API = 141.5 / SG - 131.5

            TVP = bopp.DW_CalcBubP(bopp.RET_VMOL(PropertyPackages.Phase.Mixture), 310.95, 101325)(0)

            v1 = bopp.DW_CalcViscosidadeDinamica_ISOL(PropertyPackages.Phase.Liquid, 310.95, 101325)
            kv1 = v1 / bopp.DW_CalcMassaEspecifica_ISOL(PropertyPackages.Phase.Liquid, 310.95, 101325)
            v2 = bopp.DW_CalcViscosidadeDinamica_ISOL(PropertyPackages.Phase.Liquid, 372.05, 101325)

            t10ASTM = MeABP * 0.9

        Else

            Dim ppi As New PropertyPackages.RaoultPropertyPackage
            ppi.CurrentMaterialStream = mat

            MABP = 0
            CABP = 0

            Dim i As Integer = 0
            Dim Vx(mat.Phases(0).Compounds.Count - 1) As Double
            For Each subst As Compound In mat.Phases(0).Compounds.Values
                MABP += subst.MoleFraction.GetValueOrDefault * subst.ConstantProperties.Normal_Boiling_Point
                CABP += subst.MoleFraction.GetValueOrDefault * subst.ConstantProperties.Normal_Boiling_Point ^ (1 / 3)
                Vx(i) = subst.MoleFraction
                i = i + 1
            Next
            CABP = CABP ^ 3
            MeABP = (MABP + CABP) / 2

            SG = pp.DW_CalcMassaEspecifica_ISOL(PropertyPackages.Phase.Liquid, 288.706, 101325) / 999
            K = (1.8 * MeABP) ^ (1 / 3) / SG
            API = 141.5 / SG - 131.5


            TVP = pp.DW_CalcBubP(Vx, 310.95, 101325)(4)
            v1 = pp.DW_CalcViscosidadeDinamica_ISOL(PropertyPackages.Phase.Liquid, 310.95, 101325)
            kv1 = v1 / pp.DW_CalcMassaEspecifica_ISOL(PropertyPackages.Phase.Liquid, 310.95, 101325)
            v2 = pp.DW_CalcViscosidadeDinamica_ISOL(PropertyPackages.Phase.Liquid, 372.05, 101325)

            Try
                bt = pp.DW_CalcBubT(Vx, 101325)(4)
            Catch ex As Exception
                bt = ppi.DW_CalcBubT(Vx, 101325)(4)
            End Try
            Try
                dp = pp.DW_CalcDewP(Vx, 310.95)(4)
            Catch ex As Exception
                dp = ppi.DW_CalcDewP(Vx, 310.95)(4)
            End Try

            If dp < 0 Or Double.IsNaN(dp) Or Double.IsInfinity(dp) Then
                dp = ppi.DW_CalcDewP(Vx, 310.95)(4)
            End If

            Dim tmp, vv, vl, dv, dl, mwv, mwl As Object
            Dim vwl(mat.Phases(0).Compounds.Count - 1), vwv(mat.Phases(0).Compounds.Count - 1) As Double

            Dim t, t_ant, t_ant2, ft, ft_ant, ft_ant2, v As Double, j As Integer

            t = bt + 15
            i = 0
            Do
                ft_ant2 = ft_ant
                ft_ant = ft
                Try
                    tmp = pp.FlashBase.Flash_PT(pp.RET_VMOL(PropertyPackages.Phase.Mixture), 101325, t, pp)
                    v = tmp(1)
                    vv = tmp(3)
                    vl = tmp(2)
                Catch ex As Exception
                    tmp = ppi.FlashBase.Flash_PT(pp.RET_VMOL(PropertyPackages.Phase.Mixture), 101325, t, pp)
                    v = tmp(1)
                    vv = tmp(3)
                    vl = tmp(2)
                End Try

                mwv = pp.AUX_MMM(vv)
                mwl = pp.AUX_MMM(vl)

                mat.Phases(0).Properties.temperature = t
                mat.Phases(0).Properties.pressure = 101325
                j = 0
                For Each subst As Compound In mat.Phases(1).Compounds.Values
                    subst.MoleFraction = vl(j)
                    j += 1
                Next
                pp.DW_CalcProp("density", PropertyPackages.Phase.Liquid)
                dl = mat.Phases(1).Properties.density.GetValueOrDefault
                j = 0
                For Each subst As Compound In mat.Phases(1).Compounds.Values
                    subst.MoleFraction = vv(j)
                    j += 1
                Next
                pp.DW_CalcProp("density", PropertyPackages.Phase.Liquid)
                dv = mat.Phases(1).Properties.density.GetValueOrDefault

                If v = 0 Then v = i * 0.0001

                ft = v - (0.1 / dv) / ((0.1 / dv) + (0.9 / dl))

                t_ant2 = t_ant
                t_ant = t
                If i > 2 Then
                    If ft <> ft_ant2 Then
                        t = t - 0.3 * ft * (t - t_ant2) / (ft - ft_ant2)
                    Else
                        t = t
                    End If
                Else
                    t = t - 1
                End If
                i = i + 1
            Loop Until Abs(ft) < 0.001 Or t < 0 Or Double.IsNaN(t) Or Double.IsInfinity(t) Or i > 200

            t10TBP = t

            t10ASTM = (t10TBP / 0.5564) ^ (1 / 1.09)

        End If

        'API Procedure 2B5.1
        Huang_I = 0.02266 * Exp(0.0003905 * (1.8 * MeABP) + 2.468 * SG - 0.0005704 * (1.8 * MeABP) * SG) * (1.8 * MeABP) ^ 0.0572 * SG ^ -0.72
        RefractionIndex = ((1 + 2 * Huang_I) / (1 - Huang_I)) ^ 0.5

        'API Procedure 2B7.1 (Pensky-Martens Closed Cup - ASTM D93)
        FlashPoint = 0.69 * ((t10ASTM - 273.15) * 9 / 5 + 32) - 118.2
        FlashPoint = (FlashPoint - 32) * 5 / 9 + 273.15

        'API Procedure 2B8.1
        PourPoint = 753 + 136 * (1 - Exp(-0.15 * kv1 * 1000000.0)) - 572 * SG + 0.0512 * kv1 * 1000000.0 + 0.139 * (MeABP * 1.8)
        PourPoint /= 1.8

        'API Procedure 2B11.1
        FreezingPoint = -2390.42 + 1826 * SG + 122.49 * K - 0.135 * 1.8 * MeABP
        FreezingPoint /= 1.8

        'API Procedure 2B12.1
        CloudPoint = 10 ^ (-7.41 + 5.49 * Log10(MeABP * 1.8) - 0.712 * (1.8 * MeABP) ^ 0.315 - 0.133 * SG)
        CloudPoint /= 1.8

        'API Procedure 2B13.1
        CetaneIndex = 415.26 - 7.673 * API + 0.186 * (MeABP * 1.8 - 458.67) + 3.503 * API * Log10(1.8 * MeABP - 458.67) - 193.816 * Log10(MeABP * 1.8 - 458.67)

        'Reid Vapor Pressure
        'reference: https://www.epa.gov/air-emissions-factors-and-quantification/ap-42-fifth-edition-volume-i-chapter-7-liquid-storage-0 , page 7.1-82
        'TVP = Exp((2799 / (310.95 * 1.8) - 2.227) * Log10(RVP/ 6894.76) - 7261 / (310.95 * 1.8) + 12.82)*6894.76

        RVP = (10 ^ ((Log(TVP / 6894.76) + 7261 / (310.95 * 1.8) - 12.82) / (2799 / (310.95 * 1.8) - 2.227))) * 6894.76

    End Sub

    Public Function GetUtilityType() As FlowsheetUtility Implements Interfaces.IAttachedUtility.GetUtilityType
        Return FlowsheetUtility.PetroleumProperties
    End Function

    Public Property AutoUpdate As Boolean = True Implements Interfaces.IAttachedUtility.AutoUpdate

    Public Sub LoadData(data As Dictionary(Of String, Object)) Implements Interfaces.IAttachedUtility.LoadData
        For Each item In data
            SetPropertyValue(item.Key, item.Value)
        Next
    End Sub

    Public Function SaveData() As Dictionary(Of String, Object) Implements Interfaces.IAttachedUtility.SaveData
        Dim props As New Dictionary(Of String, Object)
        For Each prop In GetPropertyList()
            props.Add(prop, GetPropertyValue(prop))
        Next
        Return props
    End Function

    Public Sub Initialize() Implements Interfaces.IAttachedUtility.Initialize

    End Sub

    Public Sub Populate() Implements Interfaces.IAttachedUtility.Populate

    End Sub

End Class
