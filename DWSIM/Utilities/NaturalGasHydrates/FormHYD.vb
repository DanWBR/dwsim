'    Copyright 2008-2014 Daniel Wagner O. de Medeiros
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

Imports System.Linq

Public Class FormHYD

    Inherits UserControl

    Implements Interfaces.IAttachedUtility

    Public m_aux As DWSIM.Utilities.HYD.AuxMethods

    Dim mat As Streams.MaterialStream
    Dim Frm As FormFlowsheet

    Public su As New SystemsOfUnits.Units
    Public cv As New SystemsOfUnits.Converter
    Public nf As String

    Dim resPC, resTC As Object
    Dim tipoPC, tipoTC As String, nomesglobal() As String

    Private Property loaded As Boolean = False

    Public Sub Initialize() Implements Interfaces.IAttachedUtility.Initialize

        Me.ComboBox1.SelectedIndex = 0

        GroupBox1.Enabled = False

        Me.m_aux = New DWSIM.Utilities.HYD.AuxMethods

        Me.Frm = AttachedTo.GetFlowsheet

        Me.ComboBox3.Items.Clear()
        Me.ComboBox3.Items.Add(AttachedTo.GraphicObject.Tag.ToString)
        Me.ComboBox3.SelectedIndex = 0
        Me.ComboBox3.Enabled = False

    End Sub

    Public Sub Populate() Implements Interfaces.IAttachedUtility.Populate

        Me.su = Frm.Options.SelectedUnitSystem
        Me.nf = Frm.Options.NumberFormat

    End Sub

    Private Sub FormHYD_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        If Not loaded Then Initialize()

    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

        If Not Me.ComboBox3.SelectedItem Is Nothing Then

            Dim gobj As Drawing.SkiaSharp.GraphicObjects.GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(Me.ComboBox3.SelectedItem, Frm.FormSurface.FlowsheetSurface)
            Me.mat = Frm.Collections.FlowsheetObjectCollection(gobj.Name)

            mat.PropertyPackage.CurrentMaterialStream = mat

            If mat.PropertyPackage.RET_VCAS().Contains("7732-18-5") Then

                Dim unif As New PropertyPackages.UNIFACPropertyPackage

                unif.CurrentMaterialStream = mat

                Dim n As Integer = mat.Phases(0).Compounds.Count - 1

                Dim Vz(n), T, P As Double
                Dim nomes(mat.Phases(0).Compounds.Count - 1) As String
                Dim comp As BaseClasses.Compound
                Dim i As Integer = 0
                For Each comp In mat.Phases(0).Compounds.Values
                    Vz(i) = comp.MoleFraction.GetValueOrDefault
                    nomes(i) = comp.Name
                    i += 1
                Next
                nomesglobal = nomes
                T = mat.Phases(0).Properties.temperature
                P = mat.Phases(0).Properties.pressure

                Dim pform(1) As Object, tform(1) As Object, PH As Double, TH As Double

                If ComboBox1.SelectedIndex = 0 Then

                    Dim hid As New DWSIM.Utilities.HYD.vdwP_PP(mat)
                    pform = hid.HYD_vdwP2(T, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(nomes), CheckBox1.Checked)
                    tform = hid.HYD_vdwP2T(P, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(nomes), CheckBox1.Checked)

                    'verificar qual estrutura se forma primeiro
                    If pform(0) <= pform(1) Then
                        tipoTC = "sI"
                        PH = pform(0)
                    Else
                        tipoTC = "sII"
                        PH = pform(1)
                    End If
                    'MsgBox(tform(0) & " " & tform(1))
                    'MsgBox(pform(0) & " " & pform(1))

                    If tform(0) >= tform(1) Then
                        tipoPC = "sI"
                        TH = tform(0)
                    Else
                        tipoPC = "sII"
                        TH = tform(1)
                    End If

                    resPC = hid.DET_HYD_vdwP(tipoPC, P, TH, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(nomes), CheckBox1.Checked)
                    resTC = hid.DET_HYD_vdwP(tipoTC, PH, T, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(nomes), CheckBox1.Checked)

                ElseIf ComboBox1.SelectedIndex = 1 Then

                    Dim hid As New DWSIM.Utilities.HYD.KlaudaSandler(mat)
                    pform = hid.HYD_KS2(T, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(nomes), CheckBox1.Checked)
                    tform = hid.HYD_KS2T(P, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(nomes), CheckBox1.Checked)

                    'verificar qual estrutura se forma primeiro
                    If pform(0) <= pform(1) Then
                        tipoTC = "sI"
                        PH = pform(0)
                    Else
                        tipoTC = "sII"
                        PH = pform(1)
                    End If
                    'MsgBox(tform(0) & " " & tform(1))
                    'MsgBox(pform(0) & " " & pform(1))
                    If tform(0) >= tform(1) Then
                        tipoPC = "sI"
                        TH = tform(0)
                    Else
                        tipoPC = "sII"
                        TH = tform(1)
                    End If

                    resPC = hid.DET_HYD_KS(tipoPC, P, TH, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(nomes), CheckBox1.Checked)
                    resTC = hid.DET_HYD_KS(tipoTC, PH, T, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(nomes), CheckBox1.Checked)

                ElseIf ComboBox1.SelectedIndex = 3 Then

                    Dim hid As New DWSIM.Utilities.HYD.KlaudaSandlerMOD(mat)
                    pform = hid.HYD_KS2(T, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(nomes), CheckBox1.Checked)
                    tform = hid.HYD_KS2T(P, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(nomes), CheckBox1.Checked)

                    'verificar qual estrutura se forma primeiro
                    If pform(0) <= pform(1) Then
                        tipoTC = "sI"
                        PH = pform(0)
                    Else
                        tipoTC = "sII"
                        PH = pform(1)
                    End If
                    'MsgBox(tform(0) & " " & tform(1))
                    'MsgBox(pform(0) & " " & pform(1))
                    If tform(0) >= tform(1) Then
                        tipoPC = "sI"
                        TH = tform(0)
                    Else
                        tipoPC = "sII"
                        TH = tform(1)
                    End If

                    If TH > 0 Then resPC = hid.DET_HYD_KS(tipoPC, P, TH, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(nomes), CheckBox1.Checked)
                    resTC = hid.DET_HYD_KS(tipoTC, PH, T, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(nomes), CheckBox1.Checked)

                ElseIf ComboBox1.SelectedIndex = 2 Then

                    Dim hid As New DWSIM.Utilities.HYD.ChenGuo(mat)
                    pform = hid.HYD_CG2(T, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(nomes), CheckBox1.Checked)
                    tform = hid.HYD_CG2T(P, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(nomes), CheckBox1.Checked)


                    'verificar qual estrutura se forma primeiro
                    If pform(0) <= pform(1) Then
                        tipoTC = "sI"
                        PH = pform(0)
                    Else
                        tipoTC = "sII"
                        PH = pform(1)
                    End If
                    'MsgBox(tform(0) & " " & tform(1))
                    'MsgBox(pform(0) & " " & pform(1))
                    If tform(0) >= tform(1) Then
                        tipoPC = "sI"
                        TH = tform(0)
                    Else
                        tipoPC = "sII"
                        TH = tform(1)
                    End If

                    resPC = hid.DET_HYD_CG(tipoPC, P, TH, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(nomes), CheckBox1.Checked)
                    resTC = hid.DET_HYD_CG(tipoTC, PH, T, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(nomes), CheckBox1.Checked)

                End If

                'unidades
                Dim uP, uT As String
                uP = su.pressure
                uT = su.temperature

                Label5.Text = uP
                Label15.Text = uP
                Label1.Text = uT
                Label16.Text = uT

                Dim PhasesTC As String = ""
                If PH > 600 * 101325 Then
                    Label8.Text = DWSIM.App.GetLocalString("ND")
                    Me.KryptonButton2.Enabled = False
                    PhasesTC = DWSIM.App.GetLocalString("ND")
                Else
                    Label8.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PH), nf)
                    Me.KryptonButton2.Enabled = True
                    If CheckBox1.Checked Then
                        PhasesTC = DWSIM.App.GetLocalString("VaporAndHydrate") & " (" & tipoTC & ")"
                    Else
                        If Math.Abs(T - resTC(0)) < 0.1 Or T = resTC(0) Then
                            PhasesTC = DWSIM.App.GetLocalString("SlidoGeloLquidoguaGs1") & tipoTC & ")"
                        ElseIf T < resTC(0) Then
                            PhasesTC = DWSIM.App.GetLocalString("SlidoGeloGseHidrato1") & tipoTC & ")"
                        ElseIf T > resTC(0) Then
                            PhasesTC = DWSIM.App.GetLocalString("LquidoguaGseHidrato") & tipoTC & ")"
                        End If
                    End If
                End If
                Dim PhasesPC As String = ""
                If TH < 0 Then
                    Label14.Text = DWSIM.App.GetLocalString("ND")
                    Me.KryptonButton3.Enabled = False
                    PhasesPC = DWSIM.App.GetLocalString("ND")
                Else
                    Label14.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TH), nf)
                    Me.KryptonButton3.Enabled = True
                    If CheckBox1.Checked Then
                        PhasesPC = DWSIM.App.GetLocalString("VaporAndHydrate") & " (" & tipoPC & ")"
                    Else
                        PhasesPC = DWSIM.App.GetLocalString("SlidoGeloGseHidrato1") & tipoPC & ")"
                        If TH > resPC(0) Then PhasesPC = DWSIM.App.GetLocalString("LquidoguaGseHidrato") & tipoPC & ")"
                        If Math.Abs(TH - resPC(0)) < 0.01 Then PhasesPC = DWSIM.App.GetLocalString("SlidoGeloLquidoguaGs1") & tipoPC & ")"
                    End If
                End If
                Label17.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, P), nf)
                Label9.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T), nf)
                Label12.Text = PhasesPC
                Label10.Text = PhasesTC

                'logica para verificar se forma hidrato ou nao
                If T <= TH Then
                    Label21.Text = DWSIM.App.GetLocalString("Sim")
                    Label20.Text = tipoTC
                ElseIf P >= PH Then
                    Label21.Text = DWSIM.App.GetLocalString("Sim")
                    Label20.Text = tipoPC
                Else
                    Label21.Text = DWSIM.App.GetLocalString("No")
                    Label20.Text = DWSIM.App.GetLocalString("NA")
                End If

                GroupBox1.Enabled = True

                unif.CurrentMaterialStream = Nothing
                unif = Nothing

            Else

                MessageBox.Show(DWSIM.App.GetLocalString("Noexisteguanacorrent"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)

            End If


        Else

            Me.mat = Nothing
            Me.LblSelected.Text = ""

        End If

    End Sub

    Private Sub KryptonButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton2.Click

        Dim frmdet As New FormHYD_DET
        With frmdet
            .res = resTC
            .P = SystemsOfUnits.Converter.ConvertToSI(su.pressure, Label8.Text)
            .T = SystemsOfUnits.Converter.ConvertToSI(su.temperature, Label9.Text)
            If Label10.ToString.Contains("sII") Then .sI = False
            .model = ComboBox1.SelectedIndex
            .nomes = nomesglobal
            .ShowDialog(Me)
        End With


    End Sub

    Private Sub KryptonButton3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton3.Click

        Dim frmdet As New FormHYD_DET
        With frmdet
            .res = resPC
            .P = SystemsOfUnits.Converter.ConvertToSI(su.pressure, Label17.Text)
            .T = SystemsOfUnits.Converter.ConvertToSI(su.temperature, Label14.Text)
            If Label12.ToString.Contains("sII") Then .sI = False
            .model = ComboBox1.SelectedIndex
            .nomes = nomesglobal
            .ShowDialog(Me)
        End With
    End Sub

    Private Sub FormHYD_HelpRequested(sender As System.Object, hlpevent As System.Windows.Forms.HelpEventArgs) Handles MyBase.HelpRequested
        DWSIM.App.HelpRequested("UT_HydrateDissociation.htm")
    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox1.SelectedIndexChanged
        If ComboBox1.SelectedIndex = 2 Then CheckBox1.Enabled = False Else CheckBox1.Enabled = True
    End Sub

    Public Property AttachedTo As Interfaces.ISimulationObject Implements Interfaces.IAttachedUtility.AttachedTo

    Public Function GetPropertyList() As List(Of String) Implements Interfaces.IAttachedUtility.GetPropertyList
        Return New List(Of String)

    End Function

    Public Function GetPropertyUnits(pname As String) As String Implements Interfaces.IAttachedUtility.GetPropertyUnits
        Return ""

    End Function

    Public Function GetPropertyValue(pname As String) As Object Implements Interfaces.IAttachedUtility.GetPropertyValue
        Return ""

    End Function

    Public Property ID As Integer Implements Interfaces.IAttachedUtility.ID

    Public Property Name1 As String Implements Interfaces.IAttachedUtility.Name

    Public Sub SetPropertyValue(pname As String, pvalue As Object) Implements Interfaces.IAttachedUtility.SetPropertyValue

    End Sub

    Public Sub Update1() Implements Interfaces.IAttachedUtility.Update

    End Sub

    Public Function GetUtilityType() As Interfaces.Enums.FlowsheetUtility Implements Interfaces.IAttachedUtility.GetUtilityType
        Return FlowsheetUtility.NaturalGasHydrates
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

End Class
