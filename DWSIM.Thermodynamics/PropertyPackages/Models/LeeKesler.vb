'    Peng-Robinson Lee-Kesler Property Package 
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

Namespace PropertyPackages.Auxiliary

    <System.Serializable()> Public Class LeeKesler

        Dim m_pr As New PropertyPackages.Auxiliary.PROPS
        '<System.NonSerialized()> Dim m_xn As DLLXnumbers.Xnumbers

        Sub New()

        End Sub

        Function MixCritProp_LK(ByVal Vz As Double(), ByVal VTc As Double(), ByVal VPc As Double(), ByVal Vw As Double())

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "MixCritProp_LK", "Lee-Kesler Mixture Critical Properties", "Lee-Kesler Mixture Critical Properties Calculation Routine")

            PopulateWithDefaultText(IObj)

            IObj?.Paragraphs.Add("<h2>Input Parameters</h2>")
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Temperatures: {0} K", VTc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Pressures: {0} Pa", VPc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Acentric Factors: {0} ", Vw.ToMathArrayString))

            Dim Pcm, Tcm, Vcm, wm As Double
            Dim n As Integer = Vz.Length - 1
            Dim VZc(n), VVc(n) As Double

            Dim i, j As Integer

            wm = 0
            i = 0
            Do
                wm += Vz(i) * Vw(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                VZc(i) = 0.2905 - 0.085 * Vw(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                VVc(i) = VZc(i) * 8.314 * VTc(i) / VPc(i)
                i = i + 1
            Loop Until i = n + 1

            Vcm = 0
            i = 0
            Do
                j = 0
                Do
                    Vcm += 1 / 8 * Vz(i) * Vz(j) * (VVc(i) ^ (1 / 3) + VVc(j) ^ (1 / 3)) ^ 3
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Tcm = 0
            i = 0
            Do
                j = 0
                Do
                    Tcm += 1 / 8 / Vcm * Vz(i) * Vz(j) * (VVc(i) ^ (1 / 3) + VVc(j) ^ (1 / 3)) ^ 3 * (VTc(i) * VTc(j)) ^ 0.5
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Pcm = (0.2905 - 0.085 * wm) * 8.314 * Tcm / Vcm


            IObj?.Paragraphs.Add("<h2>Results</h2>")
            IObj?.Paragraphs.Add(String.Format("Mixture Critical Temperature: {0} K", Tcm))
            IObj?.Paragraphs.Add(String.Format("Mixture Critical Pressure: {0} Pa", Pcm))
            IObj?.Paragraphs.Add(String.Format("Mixture Critical Volume: {0} ", Vcm))
            IObj?.Paragraphs.Add(String.Format("Mixture Acentric Factor: {0} ", wm))

            IObj?.Close()

            Return New Object() {Tcm, Pcm, Vcm, wm}

        End Function

        Function H_LK_MIX(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Double(), ByVal VKij As Double(,), ByVal VTc As Double(), ByVal VPc As Double(), ByVal Vw As Double(), ByVal VMM As Double(), ByVal Hid As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "H_LK_MIX", "Lee-Kesler Enthalpy", "Lee-Kesler Enthalpy Calculation Routine")

            PopulateWithDefaultText(IObj)

            IObj?.Paragraphs.Add("<h2>Input Parameters</h2>")
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Ideal Gas Enthalpy: {0} kJ/kg", Hid))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Interaction Parameters: {0}", VKij.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Temperatures: {0} K", VTc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Pressures: {0} Pa", VPc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Acentric Factors: {0} ", Vw.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("State: {0}", TIPO))

            Dim n, R As Double
            Dim Tc(), Pc(), w(), Tr() As Double
            Dim i As Integer

            n = Vz.Length - 1

            ReDim Tc(n), Pc(n), w(n), Tr(n)

            R = 8.314

            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                w(i) = Vw(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vz(i) * VMM(i)
                i += 1
            Loop Until i = n + 1

            IObj?.SetCurrent

            'mixture critical properties
            Dim Tcm, Pcm, wm As Double
            Dim obj = Me.MixCritProp_LK(Vz, VTc, VPc, Vw)
            Tcm = obj(0)
            Pcm = obj(1)
            wm = obj(3)

            IObj?.SetCurrent

            'Dim DHres = R * Tcm * Me.Hlk(T / Tcm, P / Pcm, wm)
            Dim DHres = R * Tcm * Me.H_LK(TIPO, T / Tcm, P / Pcm, wm)

            If DHres = 0 Then Throw New Exception("Erro no calculo da entalpia [LK].")

            If MathEx.Common.Sum(Vz) = 0.0# Then
                H_LK_MIX = 0.0#
            Else
                H_LK_MIX = Hid + DHres / MMm '/ 1000
            End If

            IObj?.Paragraphs.Add("<h2>Results</h2>")
            IObj?.Paragraphs.Add(String.Format("Specific Enthalpy: {0} kJ/kg", H_LK_MIX))

            IObj?.Close()

        End Function

        Function S_LK_MIX(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Double(), ByVal VKij As Double(,), ByVal VTc As Double(), ByVal VPc As Double(), ByVal Vw As Double(), ByVal VMM As Double(), ByVal Sid As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "S_LK_MIX", "Lee-Kesler Entropy", "Lee-Kesler Entropy Calculation Routine")

            PopulateWithDefaultText(IObj)

            IObj?.Paragraphs.Add("<h2>Input Parameters</h2>")
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Ideal Gas Entropy: {0} kJ/kg.K", Sid))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Interaction Parameters: {0}", VKij.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Temperatures: {0} K", VTc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Pressures: {0} Pa", VPc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Acentric Factors: {0} ", Vw.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("State: {0}", TIPO))

            Dim n, R As Double
            Dim Tc(), Pc(), w(), Tr() As Double
            Dim i As Integer

            n = Vz.Length - 1

            ReDim Tc(n), Pc(n), w(n), Tr(n)

            R = 8.314

            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                w(i) = Vw(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vz(i) * VMM(i)
                i += 1
            Loop Until i = n + 1

            IObj?.SetCurrent

            'mixture critical properties
            Dim Tcm, Pcm, wm As Double
            Dim obj = Me.MixCritProp_LK(Vz, VTc, VPc, Vw)
            Tcm = obj(0)
            Pcm = obj(1)
            wm = obj(3)

            IObj?.SetCurrent

            Dim DSres = R * Me.S_LK(TIPO, T / Tcm, P, Pcm, wm)

            If MathEx.Common.Sum(Vz) = 0.0# Then
                S_LK_MIX = 0.0#
            Else
                S_LK_MIX = Sid + DSres / MMm '/ 1000
            End If

            IObj?.Paragraphs.Add("<h2>Results</h2>")
            IObj?.Paragraphs.Add(String.Format("Specific Enthalpy: {0} kJ/kg", S_LK_MIX))

            IObj?.Close()

        End Function

        Function Z_LK(ByVal TIPO As String, ByVal Tr As Double, ByVal Pr As Double, ByVal w As Double)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Z_LK", "Lee-Kesler Compressibility Factor", "Lee-Kesler Compressibility Factor Calculation Routine")

            PopulateWithDefaultText(IObj)

            Dim z, zh, zs, wh, Vr As Double
            Dim B, C, D, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, beta, gamma As Double

            b1 = 0.1181193
            b2 = 0.265728
            b3 = 0.15479
            b4 = 0.030323
            c1 = 0.0236744
            c2 = 0.0186984
            c3 = 0
            c4 = 0.042724
            d1 = 0.0000155488
            d2 = 0.0000623689
            beta = 0.65392
            gamma = 0.060167

            B = b1 - b2 / Tr - b3 / Tr ^ 2 - b4 / Tr ^ 3
            C = c1 - c2 / Tr + c3 / Tr ^ 3
            D = d1 + d2 / Tr

            IObj?.SetCurrent
            Vr = Me.ESTIMAR_Vr2(TIPO, Pr, Tr, B, C, D, c4, beta, gamma)
            zs = Pr * Vr / Tr

            b1 = 0.2026579
            b2 = 0.331511
            b3 = 0.027655
            b4 = 0.203488
            c1 = 0.0313385
            c2 = 0.0503618
            c3 = 0.016901
            c4 = 0.041577
            d1 = 0.000048736
            d2 = 0.00000740336
            beta = 1.226
            gamma = 0.03754

            B = b1 - b2 / Tr - b3 / Tr ^ 2 - b4 / Tr ^ 3
            C = c1 - c2 / Tr + c3 / Tr ^ 3
            D = d1 + d2 / Tr

            IObj?.SetCurrent

            Vr = Me.ESTIMAR_Vr2(TIPO, Pr, Tr, B, C, D, c4, beta, gamma)

            zh = Pr * Vr / Tr

            wh = 0.3978

            z = zs + w / wh * (zh - zs)

            IObj?.Paragraphs.Add("<h2>Results</h2>")
            IObj?.Paragraphs.Add(String.Format("z: {0}", z))
            IObj?.Paragraphs.Add(String.Format("zs: {0}", zs))
            IObj?.Paragraphs.Add(String.Format("zh: {0}", zh))

            IObj?.Close()

            Return New Object() {z, zs, zh}

        End Function

        Function H_LK(ByVal TIPO As String, ByVal Tr As Double, ByVal Pr As Double, ByVal w As Double)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "H_LK", "Lee-Kesler Enthalpy", "Lee-Kesler Enthalpy Calculation Routine")

            PopulateWithDefaultText(IObj)

            IObj?.Paragraphs.Add("<h2>Input Parameters</h2>")
            IObj?.Paragraphs.Add(String.Format("Reduced Temperature: {0}", Tr))
            IObj?.Paragraphs.Add(String.Format("Reduced Pressure: {0}", Pr))
            IObj?.Paragraphs.Add(String.Format("Acentric Factor: {0}", w))
            IObj?.Paragraphs.Add(String.Format("State: {0}", TIPO))

            Dim zs, zh, wh, Vr, z, DHresS, DHresH, DHres As Double
            Dim E, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, beta, gamma As Double

            b1 = 0.1181193
            b2 = 0.265728
            b3 = 0.15479
            b4 = 0.030323
            c1 = 0.0236744
            c2 = 0.0186984
            c3 = 0
            c4 = 0.042724
            d1 = 0.0000155488
            d2 = 0.0000623689
            beta = 0.65392
            gamma = 0.060167

            wh = 0.3978

            Dim tmp = Me.Z_LK(TIPO, Tr, Pr, w)
            z = tmp(0)
            zs = tmp(1)

            Vr = zs * Tr / Pr

            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            DHresS = Tr * (zs - 1 - (b2 + 2 * b3 / Tr + 3 * b4 / Tr ^ 2) / (Tr * Vr) - (c2 - 3 * c3 / Tr ^ 2) / (2 * Tr * Vr ^ 2) + d2 / (5 * Tr * Vr ^ 5) + 3 * E)

            b1 = 0.2026579
            b2 = 0.331511
            b3 = 0.027655
            b4 = 0.203488
            c1 = 0.0313385
            c2 = 0.0503618
            c3 = 0.016901
            c4 = 0.041577
            d1 = 0.000048736
            d2 = 0.00000740336
            beta = 1.226
            gamma = 0.03754

            wh = 0.3978

            zh = tmp(2)

            Vr = zh * Tr / Pr

            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            DHresH = Tr * (zh - 1 - (b2 + 2 * b3 / Tr + 3 * b4 / Tr ^ 2) / (Tr * Vr) - (c2 - 3 * c3 / Tr ^ 2) / (2 * Tr * Vr ^ 2) + d2 / (5 * Tr * Vr ^ 5) + 3 * E)

            DHres = DHresS + w / wh * (DHresH - DHresS)

            IObj?.Paragraphs.Add("<h2>Results</h2>")
            IObj?.Paragraphs.Add(String.Format("DHres: {0}", DHres))
            IObj?.Paragraphs.Add(String.Format("DHresS: {0}", DHresS))
            IObj?.Paragraphs.Add(String.Format("DHresH: {0}", DHresH))

            IObj?.Close()

            Return DHres

        End Function

        Function S_LK(ByVal TIPO As String, ByVal Tr As Double, ByVal P As Double, ByVal Pc As Double, ByVal w As Double)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "S_LK", "Lee-Kesler Entropy", "Lee-Kesler Entropy Calculation Routine")

            PopulateWithDefaultText(IObj)

            IObj?.Paragraphs.Add("<h2>Input Parameters</h2>")
            IObj?.Paragraphs.Add(String.Format("Reduced Temperature: {0}", Tr))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Critical Pressure: {0} Pa", Pc))
            IObj?.Paragraphs.Add(String.Format("Acentric Factor: {0}", w))
            IObj?.Paragraphs.Add(String.Format("State: {0}", TIPO))

            Dim zs, zh, wh, Vr, z, DSresS, DSresH, DSres, Pr As Double
            Dim E, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, beta, gamma As Double

            Pr = P / Pc

            b1 = 0.1181193
            b2 = 0.265728
            b3 = 0.15479
            b4 = 0.030323
            c1 = 0.0236744
            c2 = 0.0186984
            c3 = 0
            c4 = 0.042724
            d1 = 0.0000155488
            d2 = 0.0000623689
            beta = 0.65392
            gamma = 0.060167

            wh = 0.3978

            Dim tmp = Me.Z_LK(TIPO, Tr, Pr, w)
            z = tmp(0)
            zs = tmp(1)

            Vr = zs * Tr / Pr

            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            DSresS = Math.Log(zs) - Math.Log(1) - (b1 + b3 / Tr ^ 2 + 2 * b4 / Tr ^ 3) / Vr - (c1 - 2 * c3 / Tr ^ 3) / (2 * Vr ^ 2) - d1 / (5 * Vr ^ 5) + 2 * E
            'DSresS = Math.Log(zs) - Math.Log(P / 101325) - (b1 + b3 / Tr ^ 2 + 2 * b4 / Tr ^ 3) / Vr - (c1 - 2 * c3 / Tr ^ 3) / (2 * Vr ^ 2) - d1 / (5 * Vr ^ 5) + 2 * E

            b1 = 0.2026579
            b2 = 0.331511
            b3 = 0.027655
            b4 = 0.203488
            c1 = 0.0313385
            c2 = 0.0503618
            c3 = 0.016901
            c4 = 0.041577
            d1 = 0.000048736
            d2 = 0.00000740336
            beta = 1.226
            gamma = 0.03754

            wh = 0.3978

            zh = tmp(2)

            Vr = zh * Tr / Pr

            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            DSresH = Math.Log(zh) - Math.Log(1) - (b1 + b3 / Tr ^ 2 + 2 * b4 / Tr ^ 3) / Vr - (c1 - 2 * c3 / Tr ^ 3) / (2 * Vr ^ 2) - d1 / (5 * Vr ^ 5) + 2 * E
            'DSresH = Math.Log(zh) - Math.Log(P / 101325) - (b1 + b3 / Tr ^ 2 + 2 * b4 / Tr ^ 3) / Vr - (c1 - 2 * c3 / Tr ^ 3) / (2 * Vr ^ 2) - d1 / (5 * Vr ^ 5) + 2 * E

            DSres = DSresS + w / wh * (DSresH - DSresS)

            IObj?.Paragraphs.Add("<h2>Results</h2>")
            IObj?.Paragraphs.Add(String.Format("DSres: {0}", DSres))
            IObj?.Paragraphs.Add(String.Format("DSresS: {0}", DSresS))
            IObj?.Paragraphs.Add(String.Format("DSresH: {0}", DSresH))

            IObj?.Close()

            Return DSres

        End Function

        Function ESTIMAR_Vr2(ByVal TIPO As String, ByVal Pr As Double, ByVal Tr As Double, ByVal B As Double, ByVal C As Double, ByVal D As Double, ByVal c4 As Double, ByVal beta As Double, ByVal gamma As Double)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            If IObj IsNot Nothing Then
                Inspector.Host.CheckAndAdd(IObj, "", "ESTIMAR_Vr2", "Lee-Kesler Reduced Volume", "Lee-Kesler Reduced Volume Calculation Routine")

                PopulateWithDefaultText(IObj)

                IObj?.Paragraphs.Add("<h2>Input Parameters</h2>")
                IObj?.Paragraphs.Add(String.Format("Reduced Temperature: {0}", Tr))
                IObj?.Paragraphs.Add(String.Format("Reduced Pressure: {0}", Pr))
                IObj?.Paragraphs.Add(String.Format("B: {0}", B))
                IObj?.Paragraphs.Add(String.Format("C: {0}", C))
                IObj?.Paragraphs.Add(String.Format("D: {0}", D))
                IObj?.Paragraphs.Add(String.Format("c4: {0}", c4))
                IObj?.Paragraphs.Add(String.Format("beta: {0}", beta))
                IObj?.Paragraphs.Add(String.Format("gamma: {0}", gamma))
                IObj?.Paragraphs.Add(String.Format("State: {0}", TIPO))
            End If

            Dim i As Integer

            Dim Tinf, Tsup, Vr As Double

            Dim fT, fT_inf, nsub, delta_T As Double

            If TIPO = "L" Then
                Tinf = 0.0000000001
                Tsup = 10
                nsub = 10000
            Else
                Tinf = 10000
                Tsup = 0.0000000001
                nsub = 10000
            End If

            delta_T = (Tsup - Tinf) / nsub

            i = 0
            Dim c4_tr_3 As Double = c4 / Tr ^ 3
            Do
                i = i + 1
                Vr = Tinf
                Dim Vr_2 = Vr ^ 2
                fT = Pr * Vr / Tr - (1 + B / Vr + C / Vr_2 + D / Vr ^ 5 + c4_tr_3 / Vr_2 * (beta + gamma / Vr_2) * Math.Exp(-gamma / Vr_2))
                Tinf = Tinf + delta_T
                Vr = Tinf
                Vr_2 = Vr ^ 2
                fT_inf = Pr * Vr / Tr - (1 + B / Vr + C / Vr_2 + D / Vr ^ 5 + c4_tr_3 / Vr_2 * (beta + gamma / Vr_2) * Math.Exp(-gamma / Vr_2))
            Loop Until fT * fT_inf < 0 Or i >= 10000
            Tsup = Tinf
            Tinf = Tinf - delta_T

            'metodo de Brent para encontrar Vc

            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = 100
            Dim iter2 As Integer

            aaa = Tinf
            bbb = Tsup
            ccc = Tsup

            Dim aaa_2 = aaa ^ 2
            faa = Pr * aaa / Tr - (1 + B / aaa + C / aaa_2 + D / aaa ^ 5 + c4_tr_3 / aaa_2 * (beta + gamma / aaa_2) * Math.Exp(-gamma / aaa_2))

            Dim bbb_2 = bbb ^ 2
            fbb = Pr * bbb / Tr - (1 + B / bbb + C / bbb_2 + D / bbb ^ 5 + c4_tr_3 / bbb_2 * (beta + gamma / bbb_2) * Math.Exp(-gamma / bbb_2))
            fcc = fbb
            iter2 = 0
            Do
                If (fbb > 0 And fcc > 0) Or (fbb < 0 And fcc < 0) Then
                    ccc = aaa
                    fcc = faa
                    ddd = bbb - aaa
                    eee = ddd
                End If
                If Math.Abs(fcc) < Math.Abs(fbb) Then
                    aaa = bbb
                    bbb = ccc
                    ccc = aaa
                    faa = fbb
                    fbb = fcc
                    fcc = faa
                End If
                tol11 = 0.0000001
                xmm = 0.5 * (ccc - bbb)
                If (Math.Abs(xmm) <= tol11) Or (fbb = 0) Then GoTo Final3
                'If Math.Abs(fbb) < 0.1 Then GoTo Final3
                If (Math.Abs(eee) >= tol11) And (Math.Abs(faa) > Math.Abs(fbb)) Then
                    sss = fbb / faa
                    If aaa = ccc Then
                        ppp = 2 * xmm * sss
                        qqq = 1 - sss
                    Else
                        qqq = faa / fcc
                        rrr = fbb / fcc
                        ppp = sss * (2 * xmm * qqq * (qqq - rrr) - (bbb - aaa) * (rrr - 1))
                        qqq = (qqq - 1) * (rrr - 1) * (sss - 1)
                    End If
                    If ppp > 0 Then qqq = -qqq
                    ppp = Math.Abs(ppp)
                    min11 = 3 * xmm * qqq - Math.Abs(tol11 * qqq)
                    min22 = Math.Abs(eee * qqq)
                    Dim tvar2 As Double
                    If min11 < min22 Then tvar2 = min11
                    If min11 > min22 Then tvar2 = min22
                    If 2 * ppp < tvar2 Then
                        eee = ddd
                        ddd = ppp / qqq
                    Else
                        ddd = xmm
                        eee = ddd
                    End If
                Else
                    ddd = xmm
                    eee = ddd
                End If
                aaa = bbb
                faa = fbb
                If (Math.Abs(ddd) > tol11) Then
                    bbb += ddd
                Else
                    bbb += Math.Sign(xmm) * tol11
                End If

                Dim bbb1_2 = bbb ^ 2
                fbb = Pr * bbb / Tr - (1 + B / bbb + C / bbb1_2 + D / bbb ^ 5 + c4_tr_3 / bbb1_2 * (beta + gamma / bbb1_2) * Math.Exp(-gamma / bbb1_2))
                iter2 += 1
            Loop Until iter2 = ITMAX2

Final3:

            IObj?.Paragraphs.Add("<h2>Results</h2>")
            IObj?.Paragraphs.Add(String.Format("Reduced Volume: {0}", bbb))

            IObj?.Close()

            Return bbb

        End Function

        Function CPCV_LK(ByVal TIPO As String, ByVal Tr As Double, ByVal Pr As Double, ByVal w As Double)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "S_LK", "Lee-Kesler Cp/Cv", "Lee-Kesler Cp/Cv Calculation Routine")

            PopulateWithDefaultText(IObj)

            IObj?.Paragraphs.Add("<h2>Input Parameters</h2>")
            IObj?.Paragraphs.Add(String.Format("Reduced Temperature: {0}", Tr))
            IObj?.Paragraphs.Add(String.Format("Reduced Pressure: {0}", Pr))
            IObj?.Paragraphs.Add(String.Format("Acentric Factor: {0}", w))
            IObj?.Paragraphs.Add(String.Format("State: {0}", TIPO))

            Dim zs, zh, wh, Vr, z, DCvS, DCvH, DCpS, DCpH, DCv, DCp, dPdT, dPdV As Double
            Dim E, B, C, D, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, beta, gamma As Double

            Dim R = 8.314

            b1 = 0.1181193
            b2 = 0.265728
            b3 = 0.15479
            b4 = 0.030323
            c1 = 0.0236744
            c2 = 0.0186984
            c3 = 0
            c4 = 0.042724
            d1 = 0.0000155488
            d2 = 0.0000623689
            beta = 0.65392
            gamma = 0.060167

            wh = 0.3978

            Dim tmp = Me.Z_LK(TIPO, Tr, Pr, w)
            z = tmp(0)
            zs = tmp(1)

            Vr = zs * Tr / Pr

            B = b1 - b2 / Tr - b3 / Tr ^ 2 - b4 / Tr ^ 3
            C = c1 - c2 / Tr + c3 / Tr ^ 3
            D = d1 + d2 / Tr
            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            DCvS = 2 * (b3 + 3 * b4 / Tr) / (Tr ^ 2 * Vr) - 3 * c3 / (Tr ^ 3 * Vr ^ 2) - 6 * E
            dPdT = 1 / Vr * (1 + (b1 + b3 / Tr ^ 2 + 2 * b4 / Tr ^ 3) / Vr + (c1 - 2 * c3 / Tr ^ 3) / (Vr ^ 2) + d1 / (Vr ^ 5) - 2 * c4 / (Tr ^ 3 * Vr ^ 2) * ((beta + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2)))
            dPdV = -Tr / Vr ^ 2 * (1 + 2 * B / Vr + 3 * C / Vr ^ 2 + 6 * D / Vr ^ 5 + c4 / (Tr ^ 3 * Vr ^ 2) * (3 * beta + (5 - 2 * (beta + gamma / Vr ^ 2)) * gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))
            DCpS = DCvS - 1 - Tr * dPdT ^ 2 / dPdV

            b1 = 0.2026579
            b2 = 0.331511
            b3 = 0.027655
            b4 = 0.203488
            c1 = 0.0313385
            c2 = 0.0503618
            c3 = 0.016901
            c4 = 0.041577
            d1 = 0.000048736
            d2 = 0.00000740336
            beta = 1.226
            gamma = 0.03754

            wh = 0.3978

            zh = tmp(2)

            Vr = zh * Tr / Pr

            B = b1 - b2 / Tr - b3 / Tr ^ 2 - b4 / Tr ^ 3
            C = c1 - c2 / Tr + c3 / Tr ^ 3
            D = d1 + d2 / Tr
            E = c4 / (2 * Tr ^ 3 * gamma) * (beta + 1 - (beta + 1 + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))

            DCvH = 2 * (b3 + 3 * b4 / Tr) / (Tr ^ 2 * Vr) - 3 * c3 / (Tr ^ 3 * Vr ^ 2) - 6 * E
            dPdT = 1 / Vr * (1 + (b1 + b3 / Tr ^ 2 + 2 * b4 / Tr ^ 3) / Vr + (c1 - 2 * c3 / Tr ^ 3) / (Vr ^ 2) + d1 / (Vr ^ 5) - 2 * c4 / (Tr ^ 3 * Vr ^ 2) * ((beta + gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2)))
            dPdV = -Tr / Vr ^ 2 * (1 + 2 * B / Vr + 3 * C / Vr ^ 2 + 6 * D / Vr ^ 5 + c4 / (Tr ^ 3 * Vr ^ 2) * (3 * beta + (5 - 2 * (beta + gamma / Vr ^ 2)) * gamma / Vr ^ 2) * Math.Exp(-gamma / Vr ^ 2))
            DCpH = DCvH - 1 - Tr * dPdT ^ 2 / dPdV

            DCv = DCvS + w / wh * (DCvH - DCvS)
            DCp = DCpS + w / wh * (DCpH - DCpS)

            Dim tmp2(1) As Double
            tmp2(0) = DCp
            tmp2(1) = DCv

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))

            IObj?.Paragraphs.Add(String.Format("Cp: {0} kJ/kg", tmp(1)))
            IObj?.Paragraphs.Add(String.Format("Cv: {0} kJ/kg", tmp(2)))

            IObj?.Close()

            Return tmp2

        End Function

        Function CpCvR_LK(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Double(), ByVal VKij As Double(,), ByVal Vzmass As Double(), ByVal VTc As Double(), ByVal VPc As Double(), ByVal VCpig As Double(), ByVal VMM As Double(), ByVal Vw As Double(), ByVal VZRa As Double())

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "CpCvR_LK", "Lee-Kesler Cp/Cv", "Lee-Kesler Heat Capacity Calculation Routine")

            PopulateWithDefaultText(IObj)

            IObj?.Paragraphs.Add("<h2>Input Parameters</h2>")
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Interaction Parameters: {0}", VKij.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Temperatures: {0} K", VTc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Pressures: {0} Pa", VPc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Acentric Factors: {0} ", Vw.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("State: {0}", TIPO))

            Dim n, R As Double
            Dim Tc(), Pc(), Vc(), w(), Tr() As Double
            Dim i

            n = Vz.Length - 1

            ReDim Tc(n), Pc(n), Vc(n), w(n), Tr(n)

            R = 8.314

            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                w(i) = Vw(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vz(i) * VMM(i)
                i += 1
            Loop Until i = n + 1

            Dim Cpm_ig = 0.0#
            i = 0
            Do
                Cpm_ig += Vzmass(i) * VCpig(i) * MMm
                i += 1
            Loop Until i = n + 1

            IObj?.SetCurrent

            'mixture critical properties
            Dim Tcm, Pcm, wm As Double
            Dim obj = Me.MixCritProp_LK(Vz, VTc, VPc, Vw)
            Tcm = obj(0)
            Pcm = obj(1)
            wm = obj(3)

            IObj?.SetCurrent

            Dim lkcp = Me.CPCV_LK(TIPO, T / Tcm, P / Pcm, wm)

            Dim tmp(2) As Double
            tmp(0) = (lkcp(0) * R + Cpm_ig) / (lkcp(1) * R + Cpm_ig - R)
            tmp(1) = (lkcp(0) * R + Cpm_ig) / MMm
            tmp(2) = (lkcp(1) * R + Cpm_ig - R) / MMm

            IObj?.Paragraphs.Add("<h2>Results</h2>")
            IObj?.Paragraphs.Add(String.Format("Cp/Cv: {0}", tmp(0)))
            IObj?.Paragraphs.Add(String.Format("Cp: {0} kJ/[kg.K]", tmp(1)))
            IObj?.Paragraphs.Add(String.Format("Cv: {0} kJ/[kg.K]", tmp(2)))

            IObj?.Close()

            CpCvR_LK = tmp

        End Function

        Private Sub PopulateWithDefaultText(iobj As InspectorItem)

            iobj?.Paragraphs.Add("Enthalpies, entropies and heat capacities are calculated by the 
                            Lee-Kesler model through the following equations:")

            iobj?.Paragraphs.Add("<m>\frac{H-H^{id}}{RT_{c}}=T_{r}(Z-1-\frac{b_{2}+2b_{3}/T_{r}+3b_{4}/T_{r}^{2}}{T_{r}V_{r}}-\frac{c_{2}-3c_{3}/T_{r}^{2}}{2T_{r}V_{r}^{2}}+\frac{d_{2}}{5T_{r}V_{r}^{2}}+3E)<m>")

            iobj?.Paragraphs.Add("<m>\frac{S-S^{id}}{R}+\ln(\frac{P}{P_{0}})=\ln Z-\frac{b_{2}+b_{3}/T_{r}^{2}+2b_{4}/T_{r}^{3}}{V_{r}}-\frac{c_{1}-2c_{3}/T_{r}^{3}}{2V_{r}^{2}}+\frac{d_{1}}{5V_{r}^{5}}+2E<m>")

            iobj?.Paragraphs.Add("<m>\frac{C_{v}-C_{v}^{id}}{R}=\frac{2(b_{3}+3b_{4}/T_{r})}{T_{r}^{2}V_{r}}-\frac{3c_{3}}{T_{r}^{3}V_{r}^{2}}-6E<m>")

            iobj?.Paragraphs.Add("<m>\frac{C_{p}-C_{p}^{id}}{R}=\frac{C_{v}-C_{v}^{id}}{R}-1-T_{r}\frac{(\frac{\partial P_{r}}{\partial T_{r}^ {}})_{V_{r}}^{2}}{(\frac{\partial P_{r}}{\partial V_{r}})_{T_{r}}}<m>")

            iobj?.Paragraphs.Add("<m>E=\frac{c_{4}}{2T_{r}^{3}\gamma}[\beta+1-(\beta+1+\frac{\gamma}{V_{r}^{2}})\exp(-\frac{\gamma}{V_{r}^{2}})]<m>")

            iobj?.Paragraphs.Add("An iterative method is required to calculate <mi>V_{r}</mi>. The user 
                            should always watch the values generated by DWSIM in order to 
                            detect any issues in the compressibility factors generated by the 
                            Lee-Kesler model.")

            iobj?.Paragraphs.Add("<m>Z=\frac{P_{r}V_{r}}{T_{r}}=1+\frac{B}{V_{r}}+\frac{C}{V_{r}^{2}}+\frac{D}{V_{r}^{5}}+\frac{c_{4}}{T_{r}^{3}V_{r}^{2}}(\beta+\frac{\gamma}{V_{r}^{2}})\exp(-\frac{\gamma}{V_{r}^{2}})<m>")

            iobj?.Paragraphs.Add("<m>B=b_{1}-b_{2}/T_{r}-b_{3}/T_{r}^{2}-b_{4}/T_{r}^{3}<m>")

            iobj?.Paragraphs.Add("<m>C=c_{1}-c_{2}/T_{r}+c_{3}/T_{r}^{3}<m>")

            iobj?.Paragraphs.Add("<m>D=d_{1}+d_{2}/T_{r}<m>")

            iobj?.Paragraphs.Add("Each property must be calculated based in two fluids apart from 
                            the main one, one simple and other for reference. For example, 
                            for the compressibility factor,")

            iobj?.Paragraphs.Add("<m>Z=Z^{(0)}+\frac{\omega}{\omega^{(r)}}(Z^{(r)}-Z^{(0)}),<m>")

            iobj?.Paragraphs.Add("where the (0) superscript refers to the simple fluid while the (r)
                             superscript refers to the reference fluid. This way, property 
                            calculation by the Lee-Kesler model should follow the sequence 
                            below (enthalpy calculation example):")

            iobj?.Paragraphs.Add("1. <mi>V_{r}</mi> and <mi>Z^{(0)}</mi> are calculated for the simple fluid at the 
                          fluid <mi>T_{r}</mi> and <mi>P_{r}</mi>. With the constants for the simple fluid, as shown in the table
                          , <mi>(H-H^{0})/{RT}_{c}</mi> is calculated. This term is <mi>[(H-H^{0})/{RT}_{c}]^{(0)}</mi>
                          . In this calculation, Z in the equation is <mi>Z^{(0)}</mi>.")

            iobj?.Paragraphs.Add("2. Step 1 is repeated, using the same <mi>T_{r}</mi> and <mi>P_{r}</mi>, but 
                          using the constants for the reference fluid as shown in table. 
                          With these values, the equation allows the 
                          calculation of <mi>[(H-H^{0})/{RT}_{c}]^{(r)}</mi>. In this 
                          step, <mi>Z</mi> in the equation is <mi>Z^{(r)}</mi>.")

            iobj?.Paragraphs.Add("3. Finally, one determines the residual enthalpy for the fluid of interest by")

            iobj?.Paragraphs.Add("<m>[(H-H^{0})/{RT}_{c}] = [(H-H^{0})/{RT}_{c}]^{(0)}+\frac{\omega}{\omega^{(r)}}([(H-H^{0})/{RT}_{c}]^{(r)}-[(H-H^{0})/{RT}_{c}]^{(0)}),<m>")

            iobj?.Paragraphs.Add("where <mi>\omega^{(r)}=0.3978</mi>.")

            iobj?.Paragraphs.Add("<h4>Constants for the Lee-Kesler model</h4>")
            iobj?.Paragraphs.Add("<table style='width:100%;text-align:center'>
                            <tr><th>Constant</th><th>         Simple Fluid</th><th>    Reference Fluid</th></tr>  
                            <tr><td>        <m>b_{1}<m> </td><td>          0.1181193 </td><td>        0.2026579 </td></tr>    
                            <tr><td>        <m>b_{2}<m> </td><td>           0.265728  </td><td>       0.331511      </td></tr>
                            <tr><td>        <m>b_{3}<m> </td><td>           0.154790 </td><td>        0.027655      </td></tr>
                            <tr><td>        <m>b_{4}<m></td><td>            0.030323 </td><td>        0.203488      </td></tr>
                            <tr><td>        <m>c_{1}<m>  </td><td>         0.0236744 </td><td>        0.0313385     </td></tr>
                            <tr><td>        <m>c_{2}<m></td><td>           0.0186984  </td><td>       0.0503618     </td></tr>
                            <tr><td>        <m>c_{3}<m></td><td>              0.0    </td><td>        0.016901      </td></tr>
                            <tr><td>        <m>c_{4}<m></td><td>            0.042724 </td><td>        0.041577      </td></tr>
                            <tr><td>  <m>d_{1}\times10^{4}<m></td><td>      0155488  </td><td>         0.48736      </td></tr>
                            <tr><td>  <m>d_{2}\times10^{4}<m></td><td>      0.623689 </td><td>        0.0740336     </td></tr>
                            <tr><td>        <m>\beta<m>  </td><td>          0.65392 </td><td>           1.226       </td></tr>
                            <tr><td>       <m>\gamma<m>  </td><td>          0.060167  </td><td>        0.03754      </td></tr>
                            </table>")

        End Sub

    End Class

End Namespace
