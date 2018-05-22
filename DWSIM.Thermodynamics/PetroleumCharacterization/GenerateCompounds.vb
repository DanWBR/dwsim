Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Thermodynamics.Utilities.PetroleumCharacterization.Methods
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary
Imports DWSIM.MathOps.MathEx.Interpolation
Imports DWSIM.MathOps.MathEx.GammaFunctions

Namespace Utilities.PetroleumCharacterization

    Public Class GenerateCompounds

        Private m_action As String
        Private error_v, error_p, error_cp As Integer

        Dim ccol As Dictionary(Of String, ICompound)

        Dim MW0, SG0, TB0, V10, V20, A_MW, B_MW, A_SG, B_SG, A_TB, B_TB, A_V, B_V, SGav_, MWav_, TBav_, V1av_, V2av_, SGav, MWav, TBav, V1av, V2av As Double
        Dim dMF, dMW, dSG, dTB, dMW_, dSG_, dTB_, dVA, dVB, dV1, dV2, dV1_, dV2_, q As Double()
        Dim _MW(1000), _SG(1000), _TB(1000), _V1(1000), _V2(1000), x(1000), val1, val2 As Double
        Dim MW, SG, TB, V1, V2, T1, T2 As Double
        Dim w As Double()

        Dim n As Integer

        Public m_comps As New System.Collections.Generic.Dictionary(Of String, BaseClasses.Compound)

        Public data As New List(Of String())

        Public Function GenerateCompounds(prefix As String, count As Integer, TCcorr As String, PCcorr As String, AFcorr As String, MWcorr As String, AdjustAF As Boolean, AdjustZR As Boolean, mwi As Nullable(Of Double), sgi As Nullable(Of Double),
                                                tbi As Nullable(Of Double), v1i As Nullable(Of Double), v2i As Nullable(Of Double),
                                                t1i As Nullable(Of Double), t2i As Nullable(Of Double),
                                                _mw0 As Double, _sg0 As Double, _tb0 As Double) As Dictionary(Of String, ICompound)

            If mwi Is Nothing And sgi Is Nothing And tbi Is Nothing Then
                Throw New Exception("Invalid input data. You must provide at least one assay property.")
            End If

            n = count

            Dim i As Integer

            If mwi.HasValue Then MW = mwi.GetValueOrDefault
            If sgi.HasValue Then SG = sgi.GetValueOrDefault
            If tbi.HasValue Then TB = tbi.GetValueOrDefault
            If v1i.HasValue Then V1 = v1i.GetValueOrDefault
            If v2i.HasValue Then V2 = v2i.GetValueOrDefault
            If t1i.HasValue Then T1 = t1i.GetValueOrDefault
            If t2i.HasValue Then T2 = t2i.GetValueOrDefault

            MW0 = _mw0
            SG0 = _sg0
            TB0 = _tb0

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

                        Select Case MWcorr
                            Case "Riazi (1986)"
                                dMW(i) = PropertyMethods.MW_Riazi(dTB(i), dSG(i))
                            Case "Winn (1956)"
                                dMW(i) = PropertyMethods.MW_Winn(dTB(i), dSG(i))
                            Case "Lee-Kesler (1974)"
                                dMW(i) = PropertyMethods.MW_LeeKesler(dTB(i), dSG(i))
                        End Select

                    Next

                ElseIf MW = 0 And TB = 0 Then

                    For i = 1 To n
                        dMW(i) = ((Math.Log(1.07 - dSG(i)) - 3.56073) / (-2.93886)) ^ 10
                        dTB(i) = 1080 - Math.Exp(6.97996 - 0.01964 * dMW(i) ^ (2 / 3))
                        Select Case MWcorr
                            Case "Riazi (1986)"
                                dMW(i) = PropertyMethods.MW_Riazi(dTB(i), dSG(i))
                            Case "Winn (1956)"
                                dMW(i) = PropertyMethods.MW_Winn(dTB(i), dSG(i))
                            Case "Lee-Kesler (1974)"
                                dMW(i) = PropertyMethods.MW_LeeKesler(dTB(i), dSG(i))
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

                        Select Case MWcorr
                            Case "Riazi (1986)"
                                dMW(i) = PropertyMethods.MW_Riazi(dTB(i), dSG(i))
                            Case "Winn (1956)"
                                dMW(i) = PropertyMethods.MW_Winn(dTB(i), dSG(i))
                            Case "Lee-Kesler (1974)"
                                dMW(i) = PropertyMethods.MW_LeeKesler(dTB(i), dSG(i))
                        End Select

                    Next

                ElseIf MW = 0 And SG = 0 Then

                    For i = 1 To n

                        Select Case MWcorr
                            Case "Riazi (1986)"
                                dMW(i) = PropertyMethods.MW_Riazi(dTB(i), dSG(i))
                            Case "Winn (1956)"
                                dMW(i) = PropertyMethods.MW_Winn(dTB(i), dSG(i))
                            Case "Lee-Kesler (1974)"
                                dMW(i) = PropertyMethods.MW_LeeKesler(dTB(i), dSG(i))
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

            Dim prop2 As New PetroleumCharacterization.Methods.GL
            Dim rnd As New Random
            Dim id As Integer = rnd.Next(1000, 9999)

            ccol = New Dictionary(Of String, ICompound)
            ccol.Clear()

            For i = 1 To n

                Dim cprop As New ConstantProperties()

                With cprop

                    .NBP = dTB(i)
                    .Normal_Boiling_Point = .NBP.GetValueOrDefault
                    .OriginalDB = "Petroleum Assay: " + prefix
                    .CurrentDB = "Petroleum Assay: " + prefix

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
                    Select Case TCcorr
                        Case "Riazi-Daubert (1985)"
                            .Critical_Temperature = PropertyMethods.Tc_RiaziDaubert(.NBP.GetValueOrDefault(), .PF_SG.GetValueOrDefault())
                        Case "Riazi (2005)"
                            .Critical_Temperature = PropertyMethods.Tc_Riazi(.NBP.GetValueOrDefault(), .PF_SG.GetValueOrDefault())
                        Case "Lee-Kesler (1976)"
                            .Critical_Temperature = PropertyMethods.Tc_LeeKesler(.NBP.GetValueOrDefault(), .PF_SG.GetValueOrDefault())
                        Case "Farah (2006)"
                            .Critical_Temperature = PropertyMethods.Tc_Farah(.PF_vA.GetValueOrDefault(), .PF_vB.GetValueOrDefault(), .NBP.GetValueOrDefault(), .PF_SG.GetValueOrDefault())
                    End Select

                    'Pc
                    Select Case PCcorr
                        Case "Riazi-Daubert (1985)"
                            .Critical_Pressure = PropertyMethods.Pc_RiaziDaubert(.NBP.GetValueOrDefault(), .PF_SG.GetValueOrDefault())
                        Case "Lee-Kesler (1976)"
                            .Critical_Pressure = PropertyMethods.Pc_LeeKesler(.NBP.GetValueOrDefault(), .PF_SG.GetValueOrDefault())
                        Case "Farah (2006)"
                            .Critical_Pressure = PropertyMethods.Pc_Farah(.PF_vA.GetValueOrDefault(), .PF_vB.GetValueOrDefault(), .NBP.GetValueOrDefault(), .PF_SG.GetValueOrDefault())
                    End Select

                    'Af
                    Select Case AFcorr
                        Case "Lee-Kesler (1976)"
                            .Acentric_Factor = PropertyMethods.AcentricFactor_LeeKesler(.Critical_Temperature, .Critical_Pressure, .NBP.GetValueOrDefault())
                        Case "Korsten (2000)"
                            .Acentric_Factor = PropertyMethods.AcentricFactor_Korsten(.Critical_Temperature, .Critical_Pressure, .NBP.GetValueOrDefault())
                    End Select

                    .Normal_Boiling_Point = .NBP.GetValueOrDefault()

                    .Molar_Weight = dMW(i)
                    .IsPF = 1
                    Dim trimchars As Char() = New Char() {" "c, "_"c, ","c, ";"c, ":"c}
                    .Name = prefix.Trim(trimchars) + "_NBP" + .NBP.GetValueOrDefault.ToString("N0")
                    .PF_Watson_K = (1.8 * .NBP.GetValueOrDefault) ^ (1 / 3) / .PF_SG.GetValueOrDefault
                    .Critical_Compressibility = PROPS.Zc1(.Acentric_Factor)
                    .Critical_Volume = 8314 * .Critical_Compressibility * .Critical_Temperature / .Critical_Pressure
                    .Z_Rackett = PROPS.Zc1(.Acentric_Factor)
                    If .Z_Rackett < 0 Then .Z_Rackett = 0.2

                    Dim tmp As Double() = prop2.calculate_Hf_Sf(.PF_SG.GetValueOrDefault(), .Molar_Weight, .NBP.GetValueOrDefault())

                    .IG_Enthalpy_of_Formation_25C = tmp(0)
                    .IG_Entropy_of_Formation_25C = tmp(1)
                    .IG_Gibbs_Energy_of_Formation_25C = tmp(0) - 298.15 * tmp(1)

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

            Dim dfit As New Methods.DensityFitting
            Dim prvsfit As New Methods.PRVSFitting
            Dim srkvsfit As New Methods.SRKVSFitting
            Dim nbpfit As New Methods.NBPFitting
            Dim tms As New Streams.MaterialStream("", "")
            Dim pp As PropertyPackages.PropertyPackage
            Dim fzra, fw, fprvs, fsrkvs As Double

            pp = New PropertyPackages.PengRobinsonPropertyPackage()

            For Each c As Compound In ccol.Values
                tms.Phases(0).Compounds.Add(c.Name, c)
            Next

            Dim recalcVc As Boolean = False

            i = 0
            For Each c As Compound In ccol.Values
                If AdjustAF Then
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
                If AdjustZR Then
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
                c.ConstantProperties.PR_Volume_Translation_Coefficient = 1.0#
                prvsfit._comp = c
                fprvs = prvsfit.MinimizeError()
                With c.ConstantProperties
                    If Math.Abs(fprvs) < 99.0# Then .PR_Volume_Translation_Coefficient *= fprvs Else .PR_Volume_Translation_Coefficient = 0.0#
                End With
                c.ConstantProperties.SRK_Volume_Translation_Coefficient = 1.0#
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

            Dim nm, fm, nbp, sgi2, mm, ct, cp, af, visc1, visc2, prvs, srkvs As String

            For Each subst As Compound In ccol.Values
                With subst
                    nm = .Name
                    fm = .MoleFraction.GetValueOrDefault.ToString
                    nbp = .ConstantProperties.NBP.GetValueOrDefault.ToString
                    sgi2 = .ConstantProperties.PF_SG.ToString
                    mm = .ConstantProperties.PF_MM.GetValueOrDefault.ToString
                    ct = .ConstantProperties.Critical_Temperature.ToString
                    cp = .ConstantProperties.Critical_Pressure.ToString
                    af = .ConstantProperties.Acentric_Factor.ToString
                    visc1 = .ConstantProperties.PF_v1.GetValueOrDefault.ToString
                    visc2 = .ConstantProperties.PF_v2.GetValueOrDefault.ToString
                    prvs = .ConstantProperties.PR_Volume_Translation_Coefficient.ToString
                    srkvs = .ConstantProperties.SRK_Volume_Translation_Coefficient.ToString
                End With
                data.Add(New String() {nm, fm, nbp, sgi2, mm, ct, cp, af, visc1, visc2, prvs, srkvs})
            Next

            For Each dset As String() In data
                For Each s As String In dset
                    If Double.TryParse(s, New Double) Then
                        If Double.IsNaN(Double.Parse(s)) Then
                            Throw New Exception("Invalid characterization, please try different parameters/settings and try again.")
                        End If
                    End If
                Next
            Next

            Return ccol

        End Function

        Sub DistMW()

            Dim i As Integer

            MWav_ = (MW - MW0) / MW0
            B_MW = 1
            A_MW = MWav_

            For i = 0 To 1000
                _MW(i) = (A_MW / B_MW * Math.Log(1 / (1 - (i + 0.001) / 1000))) ^ (1 / B_MW)
                x(i) = i / 1000
            Next

            ratinterpolation.buildfloaterhormannrationalinterpolant(x, 1000, 1, w)

            For i = 0 To n
                q(i) = B_MW / A_MW * polinterpolation.barycentricinterpolation(x, _MW, w, 1000, i / n) ^ B_MW
            Next

            For i = 1 To n
                val1 = polinterpolation.barycentricinterpolation(x, _MW, w, 1000, (i - 1) / n)
                val2 = polinterpolation.barycentricinterpolation(x, _MW, w, 1000, i / n)
                dMF(i) = Math.Exp(-B_MW / A_MW * val1 ^ B_MW) - Math.Exp(-B_MW / A_MW * val2 ^ B_MW)
                dMW_(i) = 1 / dMF(i) * (A_MW / B_MW) ^ (1 / B_MW) * (igammaf.incompletegammac(1 + 1 / B_MW, q(i - 1)) - igammaf.incompletegammac(1 + 1 / B_MW, q(i)))
                dMW(i) = MW0 * (1 + dMW_(i))
            Next

            MWav = MW0 * (1 + (A_MW / B_MW) ^ (1 / B_MW) * gammaf.gamma(1 + 1 / B_MW))

            Norm_dMF()

        End Sub

        Sub DistTB()

            Dim i As Integer

            TBav_ = (TB - TB0) / TB0
            B_TB = 1.5
            A_TB = (TBav_ / 0.689) ^ (3 / 2)

            For i = 0 To 1000
                _TB(i) = (A_TB / B_TB * Math.Log(1 / (1 - (i + 0.001) / 1000))) ^ (1 / B_TB)
                x(i) = i / 1000
            Next

            ratinterpolation.buildfloaterhormannrationalinterpolant(x, 1000, 1, w)

            For i = 0 To n
                q(i) = B_TB / A_TB * polinterpolation.barycentricinterpolation(x, _TB, w, 1000, i / n) ^ B_TB
            Next

            For i = 1 To n
                val1 = polinterpolation.barycentricinterpolation(x, _TB, w, 1000, (i - 1) / n)
                val2 = polinterpolation.barycentricinterpolation(x, _TB, w, 1000, i / n)
                dMF(i) = Math.Exp(-B_TB / A_TB * val1 ^ B_TB) - Math.Exp(-B_TB / A_TB * val2 ^ B_TB)
                dTB_(i) = 1 / dMF(i) * (A_TB / B_TB) ^ (1 / B_TB) * (igammaf.incompletegammac(1 + 1 / B_TB, q(i - 1)) - igammaf.incompletegammac(1 + 1 / B_TB, q(i)))
                dTB(i) = TB0 * (1 + dTB_(i))
            Next

            Norm_dMF()

        End Sub

        Sub Norm_dMF()

            Dim i As Integer

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

            Dim i As Integer

            For i = 0 To 1000
                _SG(i) = (A_SG / B_SG * Math.Log(1 / (1 - (i + 0.001) / 1000))) ^ (1 / B_SG)
                x(i) = i / 1000
            Next

            ratinterpolation.buildfloaterhormannrationalinterpolant(x, 1000, 1, w)

            For i = 0 To n
                q(i) = B_SG / A_SG * polinterpolation.barycentricinterpolation(x, _SG, w, 1000, i / n) ^ B_SG
            Next

            For i = 1 To n
                val1 = polinterpolation.barycentricinterpolation(x, _SG, w, 1000, (i - 1) / n)
                val2 = polinterpolation.barycentricinterpolation(x, _SG, w, 1000, i / n)
                dMF(i) = Math.Exp(-B_SG / A_SG * val1 ^ B_SG) - Math.Exp(-B_SG / A_SG * val2 ^ B_SG)
                dSG_(i) = 1 / dMF(i) * (A_SG / B_SG) ^ (1 / B_SG) * (igammaf.incompletegammac(1 + 1 / B_SG, q(i - 1)) - igammaf.incompletegammac(1 + 1 / B_SG, q(i)))
                dSG(i) = SG0 * (1 + dSG_(i))
            Next

            Norm_dMF()

        End Sub

        Sub DistVISC1()

            V1av_ = (V1 - V10) / V10
            B_V = 3
            A_V = (V1av_ / 0.619) ^ 3

            Dim i As Integer

            For i = 0 To 1000
                _V1(i) = (A_V / B_V * Math.Log(1 / (1 - (i + 0.001) / 1000))) ^ (1 / B_V)
                x(i) = i / 1000
            Next

            ratinterpolation.buildfloaterhormannrationalinterpolant(x, 1000, 1, w)

            For i = 0 To n
                q(i) = B_V / A_V * polinterpolation.barycentricinterpolation(x, _V1, w, 999, i / n) ^ B_V
            Next

            For i = 1 To n
                val1 = polinterpolation.barycentricinterpolation(x, _V1, w, 999, (i - 1) / n)
                val2 = polinterpolation.barycentricinterpolation(x, _V1, w, 999, i / n)
                dV1_(i) = 1 / dMF(i) * (A_V / B_V) ^ (1 / B_V) * (igammaf.incompletegammac(1 + 1 / B_V, q(i - 1)) - igammaf.incompletegammac(1 + 1 / B_V, q(i)))
                dV1(i) = V10 * (1 + dV1_(i))
            Next

        End Sub

        Sub DistVISC2()

            V2av_ = (V2 - V20) / V20
            B_V = 3
            A_V = (V2av_ / 0.619) ^ 3

            Dim i As Integer

            For i = 0 To 1000
                _V2(i) = (A_V / B_V * Math.Log(1 / (1 - (i + 0.001) / 1000))) ^ (1 / B_V)
                x(i) = i / 1000
            Next

            ratinterpolation.buildfloaterhormannrationalinterpolant(x, 1000, 1, w)

            For i = 0 To n
                q(i) = B_V / A_V * polinterpolation.barycentricinterpolation(x, _V2, w, 999, i / n) ^ B_V
            Next

            For i = 1 To n
                val1 = polinterpolation.barycentricinterpolation(x, _V2, w, 999, (i - 1) / n)
                val2 = polinterpolation.barycentricinterpolation(x, _V2, w, 999, i / n)
                dV2_(i) = 1 / dMF(i) * (A_V / B_V) ^ (1 / B_V) * (igammaf.incompletegammac(1 + 1 / B_V, q(i - 1)) - igammaf.incompletegammac(1 + 1 / B_V, q(i)))
                dV2(i) = V20 * (1 + dV2_(i))
            Next

        End Sub

    End Class

End Namespace
