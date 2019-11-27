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

Imports System.Math
Imports DWSIM.MathOps.MathEx.GammaFunctions
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary
Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.PetroleumCharacterization.Methods

Public Class FormPCBulk

    Inherits System.Windows.Forms.Form

    Public Shared Vf_l(,)

    Public su As New SystemsOfUnits.Units
    Public cv As New SystemsOfUnits.Converter
    Public nf As String

    Public frmwizard As FormSimulWizard

    Private frm As FormFlowsheet
    Private ps_vect(0, 10) As Object

    Private m_riazi As Utilities.PetroleumCharacterization.Methods.Riazi
    Private m_gl As Utilities.PetroleumCharacterization.Methods.GL
    Private m_props As PROPS

    Private m_action As String
    Private error_v, error_p, error_cp As Integer

    Dim ccol As Dictionary(Of String, Compound)

    Dim MW0, SG0, TB0, V10, V20, A_MW, B_MW, A_SG, B_SG, A_TB, B_TB, A_V, B_V, SGav_, MWav_, TBav_, V1av_, V2av_, SGav, MWav, TBav, V1av, V2av As Double
    Dim dMF, dMW, dSG, dTB, dMW_, dSG_, dTB_, dVA, dVB, dV1, dV2, dV1_, dV2_, q As Double()
    Dim _MW(1000), _SG(1000), _TB(1000), _V1(1000), _V2(1000), x(1000), val1, val2 As Double
    Dim MW, SG, TB, V1, V2, T1, T2 As Double
    Dim w As Double()

    Dim n As Integer

    Dim id As Integer

    Public m_comps As New System.Collections.Generic.Dictionary(Of String, BaseClasses.Compound)

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KButton1.Click

        n = Me.ComboBox1.SelectedItem

        Dim i As Integer

        If tb_mw.Text <> "" Then MW = Double.Parse(tb_mw.Text)
        If tb_sg.Text <> "" Then SG = Double.Parse(tb_sg.Text)
        If tb_wk.Text <> "" Then TB = SystemsOfUnits.Converter.ConvertToSI(su.temperature, Double.Parse(tb_wk.Text))
        If tb_v1.Text <> "" Then V1 = SystemsOfUnits.Converter.ConvertToSI(su.cinematic_viscosity, Double.Parse(tb_v1.Text)) Else V1 = 0.0#
        If tb_v2.Text <> "" Then V2 = SystemsOfUnits.Converter.ConvertToSI(su.cinematic_viscosity, Double.Parse(tb_v2.Text)) Else V2 = 0.0#
        If tb_t1.Text <> "" Then T1 = SystemsOfUnits.Converter.ConvertToSI(su.temperature, Double.Parse(tb_t1.Text))
        If tb_t2.Text <> "" Then T2 = SystemsOfUnits.Converter.ConvertToSI(su.temperature, Double.Parse(tb_t2.Text))

        MW0 = Double.Parse(Me.TextBoxMW0.Text)
        SG0 = Double.Parse(Me.TextBoxSG0.Text)
        TB0 = SystemsOfUnits.Converter.ConvertToSI(su.temperature, Double.Parse(Me.TextBoxTB0.Text))

        If (SG > 0.0 And SG <= SG0) Then
            MessageBox.Show("Specified value for Specific Gravity is less than the minimum value. Check inputs.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Exit Sub
        End If

        If (MW > 0.0 And MW <= MW0) Then
            MessageBox.Show("Specified value for Molar Weight is less than the minimum value. Check inputs.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Exit Sub
        End If

        If (TB > 0.0 And TB <= TB0) Then
            MessageBox.Show("Specified value for Molar Weight is less than the minimum value. Check inputs.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Exit Sub
        End If

        'Dim dMF(n), dMW(n), dSG(n), dTB(n), dMW_(n), dSG_(n), dTB_(n), dVA(n), dVB(n), dV1(n), dV2(n), dV1_(n), dV2_(n), q(n) As Double()
        Array.Resize(dMF, n + 1)
        Array.Resize(dMW, n + 1)
        Array.Resize(dSG, n + 1)
        Array.Resize(dTB, n + 1)
        Array.Resize(dMW_, n + 1)
        Array.Resize(dSG_, n + 1)
        Array.Resize(dTB_, n + 1)
        Array.Resize(dVA, n + 1)
        Array.Resize(dVB, n + 1)
        Array.Resize(dV1, n + 1)
        Array.Resize(dV2, n + 1)
        Array.Resize(dV1_, n + 1)
        Array.Resize(dV2_, n + 1)
        Array.Resize(q, n + 1)

        If MW <> 0 Then

            DistMW()

            If SG <> 0 And TB <> 0 Then

                DistSG()
                DistTB()

            ElseIf SG = 0 And TB <> 0 Then

                For i = 1 To n
                    dSG(i) = PropertyMethods.d15_Riazi(dMW(i))
                Next

                DistTB()

            ElseIf SG = 0 And TB = 0 Then

                For i = 1 To n
                    dSG(i) = PropertyMethods.d15_Riazi(dMW(i))
                    dTB(i) = 1080 - Math.Exp(6.97996 - 0.01964 * dMW(i) ^ (2 / 3))
                Next

            ElseIf SG <> 0 And TB = 0 Then

                DistSG()

                For i = 1 To n
                    dTB(i) = 1080 - Math.Exp(6.97996 - 0.01964 * dMW(i) ^ (2 / 3))
                Next

            End If

        ElseIf SG <> 0 Then

            DistSG()

            If MW <> 0 And TB <> 0 Then

                DistMW()
                DistTB()

            ElseIf MW = 0 And TB <> 0 Then

                DistTB()

                For i = 1 To n

                    Select Case Me.ComboBoxMW.SelectedItem.ToString
                        Case "Winn (1956)"
                            dMW(i) = PropertyMethods.MW_Winn(dTB(i), dSG(i))
                        Case "Riazi (1986)"
                            dMW(i) = PropertyMethods.MW_Riazi(dTB(i), dSG(i))
                        Case "Lee-Kesler (1974)"
                            dMW(i) = PropertyMethods.MW_LeeKesler(dTB(i), dSG(i))
                        Case Else
                            dMW(i) = PropertyMethods.MW_Riazi(dTB(i), dSG(i))
                    End Select

                Next

            ElseIf MW = 0 And TB = 0 Then

                For i = 1 To n
                    dMW(i) = ((Math.Log(1.07 - dSG(i)) - 3.56073) / (-2.93886)) ^ 10
                    dTB(i) = 1080 - Math.Exp(6.97996 - 0.01964 * dMW(i) ^ (2 / 3))
                    Select Case Me.ComboBoxMW.SelectedItem.ToString
                        Case "Winn (1956)"
                            dMW(i) = PropertyMethods.MW_Winn(dTB(i), dSG(i))
                        Case "Riazi (1986)"
                            dMW(i) = PropertyMethods.MW_Riazi(dTB(i), dSG(i))
                        Case "Lee-Kesler (1974)"
                            dMW(i) = PropertyMethods.MW_LeeKesler(dTB(i), dSG(i))
                        Case Else
                            dMW(i) = PropertyMethods.MW_Riazi(dTB(i), dSG(i))
                    End Select
                Next

            End If

        ElseIf TB <> 0 Then

            DistTB()

            If MW <> 0 And SG <> 0 Then

                DistMW()
                DistSG()

            ElseIf MW = 0 And SG <> 0 Then

                DistSG()

                For i = 1 To n

                    Select Case Me.ComboBoxMW.SelectedItem.ToString
                        Case "Winn (1956)"
                            dMW(i) = PropertyMethods.MW_Winn(dTB(i), dSG(i))
                        Case "Riazi (1986)"
                            dMW(i) = PropertyMethods.MW_Riazi(dTB(i), dSG(i))
                        Case "Lee-Kesler (1974)"
                            dMW(i) = PropertyMethods.MW_LeeKesler(dTB(i), dSG(i))
                        Case Else
                            dMW(i) = PropertyMethods.MW_Riazi(dTB(i), dSG(i))
                    End Select

                Next

            ElseIf MW = 0 And SG = 0 Then

                For i = 1 To n

                    Select Case Me.ComboBoxMW.SelectedItem.ToString
                        Case "Winn (1956)"
                            dMW(i) = PropertyMethods.MW_Winn(dTB(i), dSG(i))
                        Case "Riazi (1986)"
                            dMW(i) = PropertyMethods.MW_Riazi(dTB(i), dSG(i))
                        Case "Lee-Kesler (1974)"
                            dMW(i) = PropertyMethods.MW_LeeKesler(dTB(i), dSG(i))
                        Case Else
                            dMW(i) = PropertyMethods.MW_Riazi(dTB(i), dSG(i))
                    End Select

                    dSG(i) = PropertyMethods.d15_Riazi(dMW(i))

                Next

            ElseIf MW <> 0 And SG = 0 Then

                DistMW()

                For i = 1 To n
                    dSG(i) = PropertyMethods.d15_Riazi(dMW(i))
                Next

            End If

        End If

        Dim v37, v98 As Double

        v37 = PropertyMethods.Visc37_Abbott(dTB(1), dSG(1))
        v98 = PropertyMethods.Visc98_Abbott(dTB(1), dSG(1))

        V10 = PropertyMethods.ViscTwu(T1, 37.8 + 273.15, 98.9 + 273.15, v37, v98)
        V20 = PropertyMethods.ViscTwu(T2, 37.8 + 273.15, 98.9 + 273.15, v37, v98)

        If V1 <> 0 And V2 <> 0 Then

            DistVISC1()

            DistVISC2()

        ElseIf V1 = 0 And V2 <> 0 Then

            T1 = 37.8 + 273.15

            For i = 1 To n
                dV1(i) = PropertyMethods.Visc37_Abbott(dTB(i), dSG(i))
            Next

            DistVISC2()

        ElseIf V1 <> 0 And V2 = 0 Then

            T2 = 98.9 + 273.15

            For i = 1 To n
                dV2(i) = PropertyMethods.Visc98_Abbott(dTB(i), dSG(i))
            Next

            DistVISC1()

        ElseIf V1 = 0 And V2 = 0 Then

            T2 = 98.9 + 273.15
            T1 = 37.8 + 273.15

            For i = 1 To n
                dV1(i) = PropertyMethods.Visc37_Abbott(dTB(i), dSG(i))
                dV2(i) = PropertyMethods.Visc98_Abbott(dTB(i), dSG(i))
            Next

        End If

        For i = 1 To n
            dVA(i) = PropertyMethods.ViscWaltherASTM_A(T1, dV1(i), T2, dV2(i))
            dVB(i) = PropertyMethods.ViscWaltherASTM_B(T1, dV1(i), T2, dV2(i))
        Next

        Dim prop As New Thermodynamics.PropertyPackages.Auxiliary.PROPS
        Dim prop2 As New Utilities.PetroleumCharacterization.Methods.GL

        ccol = New Dictionary(Of String, Compound)
        ccol.Clear()

        For i = 1 To n

            Dim cprop As New ConstantProperties()

            With cprop

                .NBP = dTB(i)
                .Normal_Boiling_Point = .NBP
                .OriginalDB = "DWSIM"

                'SG
                .PF_SG = dSG(i)

                'VISC
                .PF_Tv1 = T1
                .PF_Tv2 = T2
                .PF_v1 = dV1(i)
                .PF_v2 = dV2(i)
                .PF_vA = dVA(i)
                .PF_vB = dVB(i)

                'MW
                .PF_MM = dMW(i)
                .Molar_Weight = dMW(i)

                'Tc
                Select Case Me.ComboBoxTC.SelectedItem.ToString
                    Case "Riazi-Daubert (1985)"
                        .Critical_Temperature = PropertyMethods.Tc_RiaziDaubert(.NBP, .PF_SG)
                    Case "Riazi (2005)"
                        .Critical_Temperature = PropertyMethods.Tc_Riazi(.NBP, .PF_SG)
                    Case "Lee-Kesler (1976)"
                        .Critical_Temperature = PropertyMethods.Tc_LeeKesler(.NBP, .PF_SG)
                    Case "Farah (2006)"
                        .Critical_Temperature = PropertyMethods.Tc_Farah(.PF_vA, .PF_vB, .NBP, .PF_SG)
                End Select

                'Pc
                Select Case Me.ComboBoxPC.SelectedItem.ToString
                    Case "Riazi-Daubert (1985)"
                        .Critical_Pressure = PropertyMethods.Pc_RiaziDaubert(.NBP, .PF_SG)
                    Case "Lee-Kesler (1976)"
                        .Critical_Pressure = PropertyMethods.Pc_LeeKesler(.NBP, .PF_SG)
                    Case "Farah (2006)"
                        .Critical_Pressure = PropertyMethods.Pc_Farah(.PF_vA, .PF_vB, .NBP, .PF_SG)
                End Select

                'Af
                Select Case Me.ComboBoxAF.SelectedItem.ToString
                    Case "Lee-Kesler (1976)"
                        .Acentric_Factor = PropertyMethods.AcentricFactor_LeeKesler(.Critical_Temperature, .Critical_Pressure, .NBP)
                    Case "Korsten (2000)"
                        .Acentric_Factor = PropertyMethods.AcentricFactor_Korsten(.Critical_Temperature, .Critical_Pressure, .NBP)
                End Select

                .Normal_Boiling_Point = .NBP

                .Molar_Weight = dMW(i)
                .IsPF = 1

                If Not Double.IsNaN(.NBP.GetValueOrDefault) Then
                    .Name = "C_" & id & "_NBP_" & CInt(.NBP.GetValueOrDefault - 273.15).ToString
                    .CAS_Number = id.ToString() & "-" & CInt(.NBP.GetValueOrDefault()).ToString()
                Else
                    .Name = "C_" & id & "_NBP_" & i.ToString()
                    .CAS_Number = id.ToString() & "-" & i.ToString()
                End If

                .PF_Watson_K = (1.8 * .NBP.GetValueOrDefault) ^ (1 / 3) / .PF_SG.GetValueOrDefault
                .Critical_Compressibility = PROPS.Zc1(.Acentric_Factor)
                .Critical_Volume = 8314 * .Critical_Compressibility * .Critical_Temperature / .Critical_Pressure
                .Z_Rackett = PROPS.Zc1(.Acentric_Factor)
                If .Z_Rackett < 0 Then .Z_Rackett = 0.2

                Dim tmp = prop2.calculate_Hf_Sf(.PF_SG, .Molar_Weight, .NBP)

                .IG_Enthalpy_of_Formation_25C = tmp(0)
                .IG_Entropy_of_Formation_25C = tmp(1)
                .IG_Gibbs_Energy_of_Formation_25C = tmp(0) - 298.15 * tmp(1)

                .Formula = "C" & CDbl(tmp(2)).ToString("N2") & "H" & CDbl(tmp(3)).ToString("N2")

                Dim methods2 As New Thermodynamics.PropertyPackages.Auxiliary.PROPS
                Dim methods As New Utilities.Hypos.Methods.HYP

                .HVap_A = methods.DHvb_Vetere(.Critical_Temperature, .Critical_Pressure, .Normal_Boiling_Point) / .Molar_Weight

                .Critical_Compressibility = PROPS.Zc1(.Acentric_Factor)
                .Critical_Volume = PROPS.Vc(.Critical_Temperature, .Critical_Pressure, .Acentric_Factor, .Critical_Compressibility)
                .Z_Rackett = PROPS.Zc1(.Acentric_Factor)
                If .Z_Rackett < 0 Then
                    .Z_Rackett = 0.2
                End If

                .Chao_Seader_Acentricity = .Acentric_Factor
                .Chao_Seader_Solubility_Parameter = ((.HVap_A * .Molar_Weight - 8.314 * .Normal_Boiling_Point) * 238.846 * PROPS.liq_dens_rackett(.Normal_Boiling_Point, .Critical_Temperature, .Critical_Pressure, .Acentric_Factor, .Molar_Weight) / .Molar_Weight / 1000000.0) ^ 0.5
                .Chao_Seader_Liquid_Molar_Volume = 1 / PROPS.liq_dens_rackett(.Normal_Boiling_Point, .Critical_Temperature, .Critical_Pressure, .Acentric_Factor, .Molar_Weight) * .Molar_Weight / 1000 * 1000000.0

                methods2 = Nothing
                methods = Nothing

                .ID = -id - i + 1

            End With

            Dim subst As New Compound(cprop.Name, "")

            With subst
                .MoleFraction = dMF(i)
                .ConstantProperties = cprop
                .Name = cprop.Name
                .PetroleumFraction = True
            End With

            ccol.Add(cprop.Name, subst)

        Next

        'Adjust Acentric Factors and Rackett parameters to fit NBP and Density

        Dim dfit As New Utilities.PetroleumCharacterization.Methods.DensityFitting
        Dim prvsfit As New Utilities.PetroleumCharacterization.Methods.PRVSFitting
        Dim srkvsfit As New Utilities.PetroleumCharacterization.Methods.SRKVSFitting
        Dim nbpfit As New Utilities.PetroleumCharacterization.Methods.NBPFitting
        Dim tms As New Streams.MaterialStream("", "")
        Dim pp As PropertyPackages.PropertyPackage
        Dim fzra, fw, fprvs, fsrkvs As Double

        If frm.Options.PropertyPackages.Count > 0 Then
            pp = frm.Options.SelectedPropertyPackage
        Else
            pp = New PropertyPackages.PengRobinsonPropertyPackage()
        End If

        For Each c As Compound In ccol.Values
            tms.Phases(0).Compounds.Add(c.Name, c)
        Next

        Dim recalcVc As Boolean = False

        i = 0
        For Each c As Compound In ccol.Values
            If Me.CheckBoxADJAF.Checked Then
                If c.ConstantProperties.Acentric_Factor < 0 Then
                    c.ConstantProperties.Acentric_Factor = 0.5
                    recalcVc = True
                End If
                With nbpfit
                    ._pp = pp
                    ._ms = tms
                    ._idx = i
                    fw = .MinimizeError()
                End With
                With c.ConstantProperties
                    c.ConstantProperties.Acentric_Factor *= fw
                    c.ConstantProperties.Z_Rackett = PROPS.Zc1(c.ConstantProperties.Acentric_Factor)
                    If .Z_Rackett < 0 Then
                        .Z_Rackett = 0.2
                        recalcVc = True
                    End If
                    .Critical_Compressibility = PROPS.Zc1(.Acentric_Factor)
                    .Critical_Volume = PROPS.Vc(.Critical_Temperature, .Critical_Pressure, .Acentric_Factor, .Critical_Compressibility)
                End With
            End If
            If Me.CheckBoxADJZRA.Checked Then
                With dfit
                    ._comp = c
                    fzra = .MinimizeError()
                End With
                With c.ConstantProperties
                    .Z_Rackett *= fzra
                    If .Critical_Compressibility < 0 Or recalcVc Then
                        .Critical_Compressibility = .Z_Rackett
                        .Critical_Volume = PROPS.Vc(.Critical_Temperature, .Critical_Pressure, .Acentric_Factor, .Critical_Compressibility)
                    End If
                End With
            End If
            c.ConstantProperties.PR_Volume_Translation_Coefficient = 1
            prvsfit._comp = c
            fprvs = prvsfit.MinimizeError()
            With c.ConstantProperties
                If Math.Abs(fprvs) < 99.0# Then .PR_Volume_Translation_Coefficient *= fprvs Else .PR_Volume_Translation_Coefficient = 0.0#
            End With
            c.ConstantProperties.SRK_Volume_Translation_Coefficient = 1
            srkvsfit._comp = c
            fsrkvs = srkvsfit.MinimizeError()
            With c.ConstantProperties
                If Math.Abs(fsrkvs) < 99.0# Then .SRK_Volume_Translation_Coefficient *= fsrkvs Else .SRK_Volume_Translation_Coefficient = 0.0#
            End With
            recalcVc = False
            i += 1
        Next

        pp = Nothing
        dfit = Nothing
        nbpfit = Nothing
        tms = Nothing

        Dim nm, fm, nbp, sgi, mm, ct, cp, af, visc1, visc2, prvs, srkvs As String

        Me.DataGridView2.Rows.Clear()
        For Each subst As Compound In ccol.Values
            With subst
                nm = .Name
                fm = Format(.MoleFraction, nf)
                nbp = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, .ConstantProperties.NBP), nf)
                sgi = Format(.ConstantProperties.PF_SG, nf)
                mm = Format(.ConstantProperties.PF_MM, nf)
                ct = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, .ConstantProperties.Critical_Temperature), nf)
                cp = Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, .ConstantProperties.Critical_Pressure), nf)
                af = Format(.ConstantProperties.Acentric_Factor, nf)
                visc1 = Format(SystemsOfUnits.Converter.ConvertFromSI(su.cinematic_viscosity, .ConstantProperties.PF_v1), "E")
                visc2 = Format(SystemsOfUnits.Converter.ConvertFromSI(su.cinematic_viscosity, .ConstantProperties.PF_v2), "E")
                prvs = Format(.ConstantProperties.PR_Volume_Translation_Coefficient, "N6")
                srkvs = Format(.ConstantProperties.SRK_Volume_Translation_Coefficient, "N6")
            End With
            Me.DataGridView2.Rows.Add(New Object() {nm, fm, nbp, sgi, mm, ct, cp, af, visc1, visc2, prvs, srkvs})
        Next

    End Sub

    Sub DistMW()

        MWav_ = (MW - MW0) / MW0
        B_MW = 1
        A_MW = MWav_

        For i = 0 To 1000
            _MW(i) = (A_MW / B_MW * Log(1 / (1 - (i + 0.001) / 1000))) ^ (1 / B_MW)
            x(i) = i / 1000
        Next

        MathEx.Interpolation.ratinterpolation.buildfloaterhormannrationalinterpolant(x, 1000, 0.5, w)

        For i = 0 To n
            q(i) = B_MW / A_MW * MathEx.Interpolation.polinterpolation.barycentricinterpolation(x, _MW, w, 1000, i / n) ^ B_MW
        Next

        For i = 1 To n
            val1 = MathEx.Interpolation.polinterpolation.barycentricinterpolation(x, _MW, w, 1000, (i - 1) / n)
            val2 = MathEx.Interpolation.polinterpolation.barycentricinterpolation(x, _MW, w, 1000, i / n)
            dMF(i) = Exp(-B_MW / A_MW * val1 ^ B_MW) - Exp(-B_MW / A_MW * val2 ^ B_MW)
            dMW_(i) = 1 / dMF(i) * (A_MW / B_MW) ^ (1 / B_MW) * (igammaf.incompletegammac(1 + 1 / B_MW, q(i - 1)) - igammaf.incompletegammac(1 + 1 / B_MW, q(i)))
            dMW(i) = MW0 * (1 + dMW_(i))
        Next

        MWav = MW0 * (1 + (A_MW / B_MW) ^ (1 / B_MW) * gammaf.gamma(1 + 1 / B_MW))

        Norm_dMF()

    End Sub

    Sub DistTB()

        TBav_ = (TB - TB0) / TB0
        B_TB = 1.5
        A_TB = (TBav_ / 0.689) ^ (3 / 2)

        For i = 0 To 1000
            _TB(i) = (A_TB / B_TB * Log(1 / (1 - (i + 0.001) / 1000))) ^ (1 / B_TB)
            x(i) = i / 1000
        Next

        MathEx.Interpolation.ratinterpolation.buildfloaterhormannrationalinterpolant(x, 1000, 0.5, w)

        For i = 0 To n
            q(i) = B_TB / A_TB * MathEx.Interpolation.polinterpolation.barycentricinterpolation(x, _TB, w, 1000, i / n) ^ B_TB
        Next

        For i = 1 To n
            val1 = MathEx.Interpolation.polinterpolation.barycentricinterpolation(x, _TB, w, 1000, (i - 1) / n)
            val2 = MathEx.Interpolation.polinterpolation.barycentricinterpolation(x, _TB, w, 1000, i / n)
            dMF(i) = Exp(-B_TB / A_TB * val1 ^ B_TB) - Exp(-B_TB / A_TB * val2 ^ B_TB)
            dTB_(i) = 1 / dMF(i) * (A_TB / B_TB) ^ (1 / B_TB) * (igammaf.incompletegammac(1 + 1 / B_TB, q(i - 1)) - igammaf.incompletegammac(1 + 1 / B_TB, q(i)))
            dTB(i) = TB0 * (1 + dTB_(i))
        Next

        Norm_dMF()

    End Sub

    Sub Norm_dMF()

        Dim sum As Double = 0
        For i = 0 To dMF.Length - 1
            If Not Double.IsNaN(dMF(i)) And Not Double.IsInfinity(dMF(i)) Then sum += dMF(i)
        Next
        For i = 0 To dMF.Length - 1
            dMF(i) /= sum
        Next

    End Sub

    Sub DistSG()

        SGav_ = (SG - SG0) / SG0
        B_SG = 3
        A_SG = (SGav_ / 0.619) ^ 3

        For i = 0 To 1000
            _SG(i) = (A_SG / B_SG * Log(1 / (1 - (i + 0.001) / 1000))) ^ (1 / B_SG)
            x(i) = i / 1000
        Next

        MathEx.Interpolation.ratinterpolation.buildfloaterhormannrationalinterpolant(x, 1000, 0.5, w)

        For i = 0 To n
            q(i) = B_SG / A_SG * MathEx.Interpolation.polinterpolation.barycentricinterpolation(x, _SG, w, 1000, i / n) ^ B_SG
        Next

        For i = 1 To n
            val1 = MathEx.Interpolation.polinterpolation.barycentricinterpolation(x, _SG, w, 1000, (i - 1) / n)
            val2 = MathEx.Interpolation.polinterpolation.barycentricinterpolation(x, _SG, w, 1000, i / n)
            dMF(i) = Exp(-B_SG / A_SG * val1 ^ B_SG) - Exp(-B_SG / A_SG * val2 ^ B_SG)
            dSG_(i) = 1 / dMF(i) * (A_SG / B_SG) ^ (1 / B_SG) * (igammaf.incompletegammac(1 + 1 / B_SG, q(i - 1)) - igammaf.incompletegammac(1 + 1 / B_SG, q(i)))
            dSG(i) = SG0 * (1 + dSG_(i))
        Next

        Norm_dMF()

    End Sub

    Sub DistVISC1()

        V1av_ = (V1 - V10) / V10
        B_V = 3
        A_V = (V1av_ / 0.619) ^ 3

        For i = 0 To 1000
            _V1(i) = (A_V / B_V * Log(1 / (1 - (i + 0.001) / 1000))) ^ (1 / B_V)
            x(i) = i / 1000
        Next

        MathEx.Interpolation.ratinterpolation.buildfloaterhormannrationalinterpolant(x, 1000, 0.5, w)

        For i = 0 To n
            q(i) = B_V / A_V * MathEx.Interpolation.polinterpolation.barycentricinterpolation(x, _V1, w, 999, i / n) ^ B_V
        Next

        For i = 1 To n
            val1 = MathEx.Interpolation.polinterpolation.barycentricinterpolation(x, _V1, w, 999, (i - 1) / n)
            val2 = MathEx.Interpolation.polinterpolation.barycentricinterpolation(x, _V1, w, 999, i / n)
            dV1_(i) = 1 / dMF(i) * (A_V / B_V) ^ (1 / B_V) * (igammaf.incompletegammac(1 + 1 / B_V, q(i - 1)) - igammaf.incompletegammac(1 + 1 / B_V, q(i)))
            dV1(i) = V10 * (1 + dV1_(i))
        Next

    End Sub

    Sub DistVISC2()

        V2av_ = (V2 - V20) / V20
        B_V = 3
        A_V = (V2av_ / 0.619) ^ 3

        For i = 0 To 1000
            _V2(i) = (A_V / B_V * Log(1 / (1 - (i + 0.001) / 1000))) ^ (1 / B_V)
            x(i) = i / 1000
        Next

        MathEx.Interpolation.ratinterpolation.buildfloaterhormannrationalinterpolant(x, 1000, 0.5, w)

        For i = 0 To n
            q(i) = B_V / A_V * MathEx.Interpolation.polinterpolation.barycentricinterpolation(x, _V2, w, 999, i / n) ^ B_V
        Next

        For i = 1 To n
            val1 = MathEx.Interpolation.polinterpolation.barycentricinterpolation(x, _V2, w, 999, (i - 1) / n)
            val2 = MathEx.Interpolation.polinterpolation.barycentricinterpolation(x, _V2, w, 999, i / n)
            dV2_(i) = 1 / dMF(i) * (A_V / B_V) ^ (1 / B_V) * (igammaf.incompletegammac(1 + 1 / B_V, q(i - 1)) - igammaf.incompletegammac(1 + 1 / B_V, q(i)))
            dV2(i) = V20 * (1 + dV2_(i))
        Next

    End Sub

    Private Sub Form8_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Me.frm = My.Application.ActiveSimulation

        Me.su = frm.Options.SelectedUnitSystem
        Me.nf = frm.Options.NumberFormat

        Dim rd As New Random

        id = rd.Next(1000, 9999)

        Me.TextBox1.Text = "OIL_" & id

        Me.ComboBox1.SelectedIndex = 9

        Me.m_riazi = New Utilities.PetroleumCharacterization.Methods.Riazi
        Me.m_gl = New Utilities.PetroleumCharacterization.Methods.GL

        'opcoes
        Me.ComboBoxAF.SelectedIndex = 0
        Me.ComboBoxMW.SelectedIndex = 0
        Me.ComboBoxSG.SelectedIndex = 0
        Me.ComboBoxTC.SelectedIndex = 0
        Me.ComboBoxPC.SelectedIndex = 0

        Me.LabelTemp1.Text = su.temperature
        Me.LabelTemp2.Text = su.temperature
        Me.LabelTemp3.Text = su.temperature
        Me.LabelTemp4.Text = su.temperature

        Me.LabelVisc1.Text = su.cinematic_viscosity
        Me.LabelVisc2.Text = su.cinematic_viscosity

        Me.tb_t1.Text = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, 37.78 + 273.15).ToString(nf)
        Me.tb_t2.Text = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, 98.89 + 273.15).ToString(nf)

        Me.TextBoxSG0.Text = (0.7).ToString(nf)
        Me.TextBoxMW0.Text = 80
        Me.TextBoxTB0.Text = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, 333).ToString(nf)

        With Me.DataGridView2.Columns
            .Item(2).HeaderText += " (" & su.temperature & ")"
            .Item(4).HeaderText += " (" & su.molecularWeight & ")"
            .Item(5).HeaderText += " (" & su.temperature & ")"
            .Item(6).HeaderText += " (" & su.pressure & ")"
            .Item(8).HeaderText += " (" & su.cinematic_viscosity & ")"
            .Item(9).HeaderText += " (" & su.cinematic_viscosity & ")"
        End With

    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KButton3.Click

        'finalize button

        Dim corr As String = Me.TextBox1.Text
        Dim tmpcomp As New BaseClasses.ConstantProperties
        Dim subst As BaseClasses.Compound
        Dim gObj As Drawing.SkiaSharp.GraphicObjects.GraphicObject = Nothing
        Dim idx As Integer = 0

        Dim myassay As SharedClasses.Utilities.PetroleumCharacterization.Assay.Assay =
            New SharedClasses.Utilities.PetroleumCharacterization.Assay.Assay(tb_mw.Text.ToDoubleFromCurrent, tb_sg.Text.ToDoubleFromCurrent,
                                                                              SystemsOfUnits.Converter.ConvertToSI(su.temperature, tb_wk.Text.ToDoubleFromCurrent),
                                                                              SystemsOfUnits.Converter.ConvertToSI(su.temperature, tb_t1.Text.ToDoubleFromCurrent),
                                                                              SystemsOfUnits.Converter.ConvertToSI(su.temperature, tb_t2.Text.ToDoubleFromCurrent),
                                                                              SystemsOfUnits.Converter.ConvertToSI(su.cinematic_viscosity, tb_v1.Text.ToDoubleFromCurrent),
                                                                              SystemsOfUnits.Converter.ConvertToSI(su.cinematic_viscosity, tb_v2.Text.ToDoubleFromCurrent))
        Dim ms As New Streams.MaterialStream("", "")
        ms.SetFlowsheet(frm)
        If frm.Options.PropertyPackages.Count > 0 Then
            ms.PropertyPackage = frm.Options.SelectedPropertyPackage
        Else
            ms.PropertyPackage = New PropertyPackages.PengRobinsonPropertyPackage()
        End If
        For Each subst In ccol.Values
            ms.Phases(0).Compounds.Add(subst.Name, subst)
            ms.Phases(1).Compounds.Add(subst.Name, New Compound(subst.Name, "") With {.ConstantProperties = subst.ConstantProperties})
            ms.Phases(2).Compounds.Add(subst.Name, New Compound(subst.Name, "") With {.ConstantProperties = subst.ConstantProperties})
            ms.Phases(3).Compounds.Add(subst.Name, New Compound(subst.Name, "") With {.ConstantProperties = subst.ConstantProperties})
            ms.Phases(4).Compounds.Add(subst.Name, New Compound(subst.Name, "") With {.ConstantProperties = subst.ConstantProperties})
            ms.Phases(5).Compounds.Add(subst.Name, New Compound(subst.Name, "") With {.ConstantProperties = subst.ConstantProperties})
            ms.Phases(6).Compounds.Add(subst.Name, New Compound(subst.Name, "") With {.ConstantProperties = subst.ConstantProperties})
            ms.Phases(7).Compounds.Add(subst.Name, New Compound(subst.Name, "") With {.ConstantProperties = subst.ConstantProperties})
        Next

        Dim qc As New QualityCheck(myassay, ms)
        qc.DoQualityCheck()
        qc.DisplayForm(Sub(c)
                           Dim f As New FormPureComp() With {.Flowsheet = frm, .Added = False, .MyCompound = c}
                           f.ShowDialog()
                       End Sub,
                       Sub()
                           If Not frm.FrmStSim1.initialized Then frm.FrmStSim1.Init()

                           For Each subst In ccol.Values
                               tmpcomp = subst.ConstantProperties
                               frm.Options.NotSelectedComponents.Add(tmpcomp.Name, tmpcomp)
                               idx = frm.FrmStSim1.AddCompToGrid(tmpcomp)
                               If frmwizard IsNot Nothing Then
                                   frmwizard.AddCompToGrid(tmpcomp)
                                   frmwizard.ogc1.Sort(frmwizard.colAdd, System.ComponentModel.ListSortDirection.Descending)
                               End If
                               frm.FrmStSim1.AddCompToSimulation(tmpcomp.Name)
                           Next

                           Dim myMStr As New Drawing.SkiaSharp.GraphicObjects.Shapes.MaterialStreamGraphic(100, 100, 20, 20)
                           myMStr.LineWidth = 2
                           myMStr.Fill = True
                           myMStr.Tag = corr
                           gObj = myMStr
                           gObj.Name = "MAT-" & Guid.NewGuid.ToString
                           'OBJETO DWSIM
                           Dim myCOMS As Streams.MaterialStream = New Streams.MaterialStream(myMStr.Name, DWSIM.App.GetLocalString("CorrentedeMatria"))
                           myCOMS.GraphicObject = myMStr
                           myMStr.Owner = myCOMS
                           frm.AddComponentsRows(myCOMS)
                           If frm.Options.PropertyPackages.Count > 0 Then
                               myCOMS.PropertyPackage = frm.Options.SelectedPropertyPackage
                           Else
                               myCOMS.PropertyPackage = New PropertyPackages.PengRobinsonPropertyPackage()
                           End If
                           myCOMS.ClearAllProps()
                           Dim wtotal As Double = 0
                           For Each subst In ccol.Values
                               wtotal += subst.MoleFraction.GetValueOrDefault * subst.ConstantProperties.Molar_Weight
                           Next
                           For Each subst In ccol.Values
                               subst.MassFraction = subst.MoleFraction.GetValueOrDefault * subst.ConstantProperties.Molar_Weight / wtotal
                           Next
                           For Each subst In ccol.Values
                               With myCOMS.Phases(0).Compounds
                                   .Item(subst.Name).ConstantProperties = subst.ConstantProperties
                                   .Item(subst.Name).MassFraction = subst.MassFraction
                                   .Item(subst.Name).MoleFraction = subst.MoleFraction
                               End With
                               myCOMS.Phases(1).Compounds.Item(subst.Name).ConstantProperties = subst.ConstantProperties
                               myCOMS.Phases(2).Compounds.Item(subst.Name).ConstantProperties = subst.ConstantProperties
                           Next
                           myCOMS.SetFlowsheet(frm)
                           frm.AddSimulationObject(myCOMS)
                           frm.AddGraphicObject(gObj)
                           frm.FormSurface.Invalidate()

                           Me.Close()

                       End Sub)

    End Sub

    Private Sub FormPCBulk_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        Me.su = frm.Options.SelectedUnitSystem
        Me.nf = frm.Options.NumberFormat
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click

        'salvar ensaio

        Try
            Dim myassay As SharedClasses.Utilities.PetroleumCharacterization.Assay.Assay = New SharedClasses.Utilities.PetroleumCharacterization.Assay.Assay(tb_mw.Text, tb_sg.Text, tb_wk.Text, SystemsOfUnits.Converter.ConvertToSI(su.temperature, tb_t1.Text), SystemsOfUnits.Converter.ConvertToSI(su.temperature, tb_t2.Text), SystemsOfUnits.Converter.ConvertToSI(su.cinematic_viscosity, tb_v1.Text), SystemsOfUnits.Converter.ConvertToSI(su.cinematic_viscosity, tb_v2.Text))
            myassay.Name = Me.TextBox1.Text
            frm.Options.PetroleumAssays.Add(Guid.NewGuid().ToString, myassay)
            MessageBox.Show("Assay data was saved succesfully.", "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Information)
        Catch ex As Exception
            MessageBox.Show("Error saving assay data:" & vbCrLf & ex.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try

    End Sub

    Private Sub Button1_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

        'carregar ensaio

        Dim frmam As New FormAssayManager

        If frmam.ShowDialog(Me) = Windows.Forms.DialogResult.OK Then

            Dim myassay As SharedClasses.Utilities.PetroleumCharacterization.Assay.Assay = frmam.currentassay

            If Not myassay Is Nothing Then

                Me.tb_mw.Text = Format(myassay.MW, nf)
                Me.tb_sg.Text = Format(myassay.SG60, nf)
                Me.tb_t1.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, myassay.T1), nf)
                Me.tb_t2.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, myassay.T2), nf)
                Me.tb_wk.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, myassay.NBPAVG), nf)
                Me.tb_v1.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.cinematic_viscosity, myassay.V1), nf)
                Me.tb_v2.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.cinematic_viscosity, myassay.V2), nf)

            End If

            Try
                frmam.Close()
            Catch ex As Exception

            End Try

        End If

    End Sub

End Class