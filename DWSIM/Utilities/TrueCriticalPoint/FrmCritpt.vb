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

Imports System
Imports System.ComponentModel
Imports DWSIM.Thermodynamics


Public Class FrmCritpt

    Inherits UserControl

    Implements Interfaces.IAttachedUtility

    Dim mat As Streams.MaterialStream
    Dim Frm As FormFlowsheet

    Dim cp As Utilities.TCP.Methods
    Dim cps As Utilities.TCP.Methods_SRK

    Public su As New SystemsOfUnits.Units
    Public cv As New SystemsOfUnits.Converter
    Public nf As String

    Public Property CriticalPressure As Double
    Public Property CriticalTemperature As Double
    Public Property CriticalVolume As Double
    Public Property CriticalCompressibility As Double

    Public Property PseudoCriticalPressure As Double
    Public Property PseudoCriticalTemperature As Double
    Public Property PseudoCriticalVolume As Double
    Public Property PseudoCriticalCompressibility As Double

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

        Grid1.Rows.Clear()

        If Not Me.ComboBox3.SelectedItem Is Nothing Then

            Dim gobj As Drawing.SkiaSharp.GraphicObjects.GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(Me.ComboBox3.SelectedItem, Frm.FormSurface.FlowsheetSurface)
            Me.mat = Frm.Collections.FlowsheetObjectCollection(gobj.Name)
            Dim pr As PropertyPackages.PropertyPackage

            pr = mat.PropertyPackage
            pr.CurrentMaterialStream = mat

            Dim n As Integer = mat.Phases(0).Compounds.Count - 1

            Dim Vz(n) As Double
            Dim comp As BaseClasses.Compound
            Dim i As Integer = 0
            For Each comp In mat.Phases(0).Compounds.Values
                Vz(i) = comp.MoleFraction.GetValueOrDefault
                i += 1
            Next

            Dim j, k, l As Integer
            i = 0
            Do
                If Vz(i) = 0 Then j += 1
                i = i + 1
            Loop Until i = n + 1

            Dim VTc(n), Vpc(n), Vw(n), VVc(n), VKij(n, n) As Double
            Dim Vm2(UBound(Vz) - j), VPc2(UBound(Vz) - j), VTc2(UBound(Vz) - j), VVc2(UBound(Vz) - j), Vw2(UBound(Vz) - j), VKij2(UBound(Vz) - j, UBound(Vz) - j) As Double

            VTc = pr.RET_VTC()
            Vpc = pr.RET_VPC()
            VVc = pr.RET_VVC()
            Vw = pr.RET_VW()
            VKij = pr.RET_VKij

            i = 0
            k = 0
            Do
                If Vz(i) <> 0 Then
                    Vm2(k) = Vz(i)
                    VTc2(k) = VTc(i)
                    VPc2(k) = Vpc(i)
                    VVc2(k) = VVc(i)
                    Vw2(k) = Vw(i)
                    j = 0
                    l = 0
                    Do
                        If Vz(l) <> 0 Then
                            VKij2(k, j) = VKij(i, l)
                            j = j + 1
                        End If
                        l = l + 1
                    Loop Until l = n + 1
                    k = k + 1
                End If
                i = i + 1
            Loop Until i = n + 1

            'Try

            Dim pc As ArrayList, tmp As Object

            If mat.PropertyPackage.ComponentName.Contains("Peng-Robinson (PR)") Then

                Me.cp = New Utilities.TCP.Methods
                pc = Me.cp.CRITPT_PR(Vm2, VTc2, VPc2, VVc2, Vw2, VKij2)

            ElseIf mat.PropertyPackage.ComponentName.Contains("SRK") Then

                Me.cps = New Utilities.TCP.Methods_SRK
                pc = Me.cps.CRITPT_PR(Vm2, VTc2, VPc2, VVc2, Vw2, VKij2)

            Else

                pc = New ArrayList(mat.PropertyPackage.DW_CalculateCriticalPoints())

            End If

            Dim ppc, ptc, pvc, pzc, tpc, ttc, tvc, tzc As Double

            ppc = Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, pr.AUX_PCM(PropertyPackages.Phase.Mixture)), nf)
            ptc = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, pr.AUX_TCM(PropertyPackages.Phase.Mixture)), nf)
            pvc = Format(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, pr.AUX_VCM(PropertyPackages.Phase.Mixture) * 1000), nf)
            pzc = Format(pr.AUX_ZCM(PropertyPackages.Phase.Mixture), nf)

            PseudoCriticalTemperature = pr.AUX_TCM(PropertyPackages.Phase.Mixture)
            PseudoCriticalPressure = pr.AUX_PCM(PropertyPackages.Phase.Mixture)
            PseudoCriticalVolume = pr.AUX_VCM(PropertyPackages.Phase.Mixture) * 1000
            PseudoCriticalCompressibility = pr.AUX_ZCM(PropertyPackages.Phase.Mixture)

            Grid1.Rows.Add(New Object() {Grid1.Rows.Count + 1, "PCP", ptc, ppc, pvc, pzc})

            If pc.Count > 0 Then

                tmp = pc(0)

                CriticalTemperature = tmp(0)
                CriticalPressure = tmp(1)
                CriticalVolume = tmp(2) * 1000
                CriticalCompressibility = tmp(1) * tmp(2) / (8.314 * tmp(0))

                For Each tmp In pc
                    ttc = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, tmp(0)), nf)
                    tpc = Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, tmp(1)), nf)
                    tvc = Format(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, tmp(2) * 1000), nf)
                    tzc = Format(tmp(1) * tmp(2) / (8.314 * tmp(0)), nf)
                    Grid1.Rows.Add(New Object() {Grid1.Rows.Count + 1, "TCP", ttc, tpc, tvc, tzc})
                Next

            End If

            pc = Nothing

        Else

            Me.mat = Nothing
            Me.LblSelected.Text = ""

        End If

    End Sub

    Private Sub FrmCritpt_HelpRequested(sender As System.Object, hlpevent As System.Windows.Forms.HelpEventArgs) Handles MyBase.HelpRequested
        DWSIM.App.HelpRequested("UT_TrueCriticalPoint.htm")
    End Sub

    Private Sub FrmCritpt_Shown(sender As Object, e As EventArgs) Handles Me.Load

        Me.Frm = AttachedTo.GetFlowsheet
        mat = AttachedTo

        Me.Text = DWSIM.App.GetLocalString("DWSIMUtilitriosPonto")

        Me.su = Frm.Options.SelectedUnitSystem
        Me.nf = Frm.Options.NumberFormat

        Me.ComboBox3.Items.Clear()
        Me.ComboBox3.Items.Add(AttachedTo.GraphicObject.Tag.ToString)
        Me.ComboBox3.SelectedIndex = 0
        Me.ComboBox3.Enabled = False

        With Me.Grid1.Columns
            .Item(2).HeaderText = "Tc (" & su.temperature & ")"
            .Item(3).HeaderText = "Pc (" & su.pressure & ")"
            .Item(4).HeaderText = "Vc (" & su.molar_volume & ")"
        End With

        ExtensionMethods.FormExtensions.ChangeDefaultFont(Me)

    End Sub

    Public Property AttachedTo As Interfaces.ISimulationObject Implements Interfaces.IAttachedUtility.AttachedTo

    Public Function GetPropertyList() As List(Of String) Implements Interfaces.IAttachedUtility.GetPropertyList

        Dim plist As New List(Of String)

        plist.Add("Critical Pressure")
        plist.Add("Critical Temperature")
        plist.Add("Critical Volume")
        plist.Add("Critical Compressibility")
        plist.Add("Pseudo-Critical Pressure")
        plist.Add("Pseudo-Critical Temperature")
        plist.Add("Pseudo-Critical Volume")
        plist.Add("Pseudo-Critical Compressibility")

        Return plist

    End Function

    Public Function GetPropertyUnits(pname As String) As String Implements Interfaces.IAttachedUtility.GetPropertyUnits
        Select Case pname
            Case "Critical Pressure", "Pseudo-Critical Pressure"
                Return AttachedTo.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem.pressure
            Case "Critical Temperature", "Pseudo-Critical Temperature"
                Return AttachedTo.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem.temperature
            Case "Critical Volume", "Pseudo-Critical Volume"
                Return AttachedTo.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem.molar_volume
            Case Else
                Return ""
        End Select
    End Function

    Public Function GetPropertyValue(pname As String) As Object Implements Interfaces.IAttachedUtility.GetPropertyValue
        Dim units = AttachedTo.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem
        Select Case pname
            Case "Critical Pressure"
                Return SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(units.pressure, CriticalPressure)
            Case "Critical Temperature"
                Return SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(units.temperature, CriticalTemperature)
            Case "Critical Volume"
                Return SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(units.molar_volume, CriticalVolume)
            Case "Critical Compressibility"
                Return CriticalCompressibility
            Case "Pseudo-Critical Pressure"
                Return SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(units.pressure, PseudoCriticalPressure)
            Case "Pseudo-Critical Temperature"
                Return SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(units.temperature, PseudoCriticalTemperature)
            Case "Pseudo-Critical Volume"
                Return SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(units.molar_volume, PseudoCriticalVolume)
            Case "Pseudo-Critical Compressibility"
                Return PseudoCriticalCompressibility
            Case Else
                Return ""
        End Select
    End Function

    Public Property ID As Integer Implements Interfaces.IAttachedUtility.ID

    Public Property Name1 As String Implements Interfaces.IAttachedUtility.Name

    Public Sub SetPropertyValue(pname As String, pvalue As Object) Implements Interfaces.IAttachedUtility.SetPropertyValue

    End Sub

    Public Sub Update1() Implements Interfaces.IAttachedUtility.Update
        Button1_Click(Me, New EventArgs)
    End Sub

    Public Function GetUtilityType() As Interfaces.Enums.FlowsheetUtility Implements Interfaces.IAttachedUtility.GetUtilityType
        Return FlowsheetUtility.TrueCriticalPoint
    End Function

    Public Property AutoUpdate As Boolean Implements Interfaces.IAttachedUtility.AutoUpdate

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

        ExtensionMethods.ChangeDefaultFont(Me)

    End Sub

    Public Sub Populate() Implements Interfaces.IAttachedUtility.Populate

    End Sub
End Class