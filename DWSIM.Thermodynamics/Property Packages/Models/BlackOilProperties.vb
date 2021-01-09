
'    Black Oil Property Package additional routines
'    Copyright 2015 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU Lesser General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports System.Math
Imports props1 = DWSIM.Thermodynamics.PetroleumCharacterization.Methods.PropertyMethods

Namespace PropertyPackages.Auxiliary
    <System.Serializable> Public Class BlackOilFluid

        Public SGG, SGO, BSW, GOR, v1, t1, v2, t2, PNA_P, PNA_N, PNA_A As Double

        Sub New()

        End Sub

    End Class

    <System.Serializable> Public Class BlackOilProperties

        Private water As New Auxiliary.IAPWS_IF97

        Sub New()


        End Sub

        Public Function LiquidNormalBoilingPoint(SGO As Double, BSW As Double) As Double
            Dim MW As Double = LiquidMolecularWeight(SGO, 0)
            Return (100 - BSW) / 100 * (1080 - Math.Exp(6.97996 - 0.01964 * MW ^ (2 / 3))) + BSW / 100 * 373.15
        End Function
        Public Function LiquidMolecularWeight(SGO As Double, BSW As Double) As Double
            Return (100 - BSW) / 100 * (((Math.Log(1.07 - SGO) - 3.56073) / (-2.93886)) ^ 10) + BSW / 100 * 18
        End Function
        Public Function VaporPressure(T As Double, SGO As Double, BSW As Double) As Double

            Dim Tc, Pc, w As Double

            Dim MW As Double = LiquidMolecularWeight(SGO, 0)

            Dim NBP As Double = 1080 - Math.Exp(6.97996 - 0.01964 * MW ^ (2 / 3))

            Tc = props1.Tc_LeeKesler(NBP, SGO)
            Pc = props1.Pc_LeeKesler(NBP, SGO)
            w = props1.AcentricFactor_LeeKesler(Tc, Pc, NBP)

            Dim tmp, f0, f1 As Double

            Dim Tr As Double = T / Tc

            f0 = 5.92714 - 6.09648 / Tr - 1.28862 * Math.Log(Tr) + 0.169347 * Tr ^ 6
            f1 = 15.2518 - 15.6875 / Tr - 13.4721 * Math.Log(Tr) + 0.43577 * Tr ^ 6

            tmp = Pc * Math.Exp(f0 + w * f1)

            Return (100 - BSW) / 100 * tmp + BSW / 100 * water.pSatW(T)

        End Function
        Public Function LiquidEnthalpy(T As Double, P As Double, SGO As Double, SGG As Double, BSW As Double) As Double

            Dim methods As New Utilities.Hypos.Methods.HYP
            Dim Tc, Pc, MW As Double
            Dim NBP As Double = LiquidNormalBoilingPoint(SGO, 0)

            Tc = props1.Tc_LeeKesler(NBP, SGO)
            Pc = props1.Pc_LeeKesler(NBP, SGO)
            MW = VaporMolecularWeight(SGG)

            Dim DHvap As Double
            DHvap = methods.DHvb_Vetere(Tc, Pc, NBP) / MW

            Dim Hid As Double = 0.0#
            Dim deltaT As Double = (T - 298.15) / 10
            Dim i As Integer
            Dim Ti As Double

            Ti = 298.15
            For i = 0 To 9
                Hid += VaporCp(Ti, P, SGG, SGO) * deltaT
                Ti += deltaT
            Next

            Return (100 - BSW) / 100 * (Hid - DHvap) + BSW / 100 * water.enthalpyW(T, P / 100000)

        End Function
        Public Function LiquidDensity(T As Double, P As Double, SGO As Double, SGG As Double, GOR As Double, BSW As Double) As Double

            Dim API, WOR, GORss As Double

            Dim Tf, Trank, Ppsia As Double

            Tf = (T - 273.15) * 9 / 5 + 32
            Trank = Tf + 459.67
            Ppsia = P * 0.000145038

            API = 141.5 / SGO - 131.5

            WOR = BSW / (100 - BSW)

            GORss = GOR * 5.6738

            Dim Rs, Pb As Double

            Rs = SGG * (((Ppsia) / 18.2 + 1.4) * 10 ^ (0.0125 * API - 0.00091 * Tf)) ^ 1.2048

            Pb = 18.2 * ((GORss / SGG) ^ (1 / 1.2048) * 10 ^ (0.00091 * Tf - 0.0125 * API) - 1.4)

            Dim Bos As Double

            Bos = 0.9759 + 0.00012 * (Rs * (SGG / SGO) ^ 0.5 + 1.25 * Tf) ^ 1.2

            Dim rhoo, rhoo0 As Double

            rhoo0 = SGO * 997

            rhoo = (rhoo0 + Rs / 5.6738) / Bos

            Return (100 - BSW) / 100 * rhoo + BSW / 100 * water.densSatLiqTW(T)

        End Function
        Public Function LiquidCp(T As Double, P As Double, SGG As Double, SGO As Double, BSW As Double) As Double
            Dim Tc, Pc, w, MW As Double
            Dim NBP As Double = LiquidNormalBoilingPoint(SGO, 0)
            Tc = props1.Tc_LeeKesler(NBP, SGO)
            Pc = props1.Pc_LeeKesler(NBP, SGO)
            w = props1.AcentricFactor_LeeKesler(Tc, Pc, NBP)
            MW = LiquidMolecularWeight(SGO, 0)
            Return (100 - BSW) / 100 * PROPS.Cpl_rb(VaporCp(T, P, SGG, SGO), T, Tc, w, MW) + BSW / 100 * water.cpSatLiqTW(T)
        End Function
        Public Function LiquidCv(T As Double, P As Double, SGG As Double, SGO As Double, BSW As Double) As Double
            Return LiquidCp(T, P, SGG, SGO, BSW)
        End Function
        Public Function LiquidThermalConductivity(T As Double, P As Double, SGO As Double, BSW As Double) As Double
            Dim Tc, Pc, w, MW As Double
            Dim NBP As Double = LiquidNormalBoilingPoint(SGO, 0)
            Tc = props1.Tc_LeeKesler(NBP, SGO)
            Pc = props1.Pc_LeeKesler(NBP, SGO)
            w = props1.AcentricFactor_LeeKesler(Tc, Pc, NBP)
            MW = LiquidMolecularWeight(SGO, 0)
            Return (100 - BSW) / 100 * PROPS.condl_latini(T, NBP, Tc, MW, "H") + BSW / 100 * water.thconSatLiqTW(T)
        End Function
        Public Function LiquidViscosity(T As Double, P As Double, SGO As Double, SGG As Double, GOR As Double, BSW As Double, v1 As Double, t1 As Double, v2 As Double, t2 As Double) As Double

            If v1 <> 0 Then

                Return props1.ViscTwu(T, t1, t2, v1, v2) * LiquidDensity(T, P, SGO, SGG, GOR, BSW)

            Else

                Dim API, WOR, GORss As Double

                Dim Tf, Trank, Ppsia As Double

                Tf = (T - 273.15) * 9 / 5 + 32
                Trank = Tf + 459.67
                Ppsia = P * 0.000145038

                API = 141.5 / SGO - 131.5

                WOR = BSW / (100 - BSW)

                GORss = GOR * 5.6738

                Dim Rs, Pb As Double

                Rs = SGG * (((Ppsia) / 18.2 + 1.4) * 10 ^ (0.0125 * API - 0.00091 * Tf)) ^ 1.2048

                Pb = 18.2 * ((GORss / SGG) ^ (1 / 1.2048) * 10 ^ (0.00091 * Tf - 0.0125 * API) - 1.4)

                Dim Bos As Double

                Bos = 0.9759 + 0.00012 * (Rs * (SGG / SGO) ^ 0.5 + 1.25 * Tf) ^ 1.2

                Dim Tsep, Psep As Double

                Tsep = Tf
                Psep = Ppsia

                Dim muod, muos, muoss, muo, muossat As Double

                muod = -1 + 10 ^ (10 ^ (1.8653 - 0.025086 * API - 0.5644 * Log(Tf) / Log(10)))

                muos = 10.715 * (Rs + 100) ^ -0.515 * (muod) ^ (5.44 * (Rs + 150) ^ -0.338)

                muossat = 10.715 * (GORss + 100) ^ -0.515 * (muod) ^ (5.44 * (GORss + 150) ^ -0.338)

                muoss = muossat * (Pb / Ppsia) ^ (2.6 * Ppsia ^ 1.187 * 10 ^ (-0.000039 * Ppsia - 5))

                If Ppsia < Pb Then muo = muos Else muo = muoss

                Return (100 - BSW) / 100 * (muo * 0.001) + BSW / 100 * water.viscSatLiqTW(T)

            End If

        End Function
        Public Function VaporMolecularWeight(SGG As Double) As Double
            Return SGG * 28.97
        End Function
        Public Function VaporEnthalpy(T As Double, P As Double, SGG As Double, SGO As Double) As Double

            Dim Hid As Double = 0.0#
            Dim deltaT As Double = (T - 298.15) / 10
            Dim i As Integer
            Dim Ti As Double
            Ti = 298.15
            For i = 0 To 9
                Hid += VaporCp(Ti, P, SGG, SGO) * deltaT
                Ti += deltaT
            Next

            Return Hid

        End Function
        Public Function VaporDensity(T As Double, P As Double, SGG As Double) As Double
            Dim val As Double
            val = VaporCompressibilityFactor(T, P, SGG)
            val = (8.314 * val * T / P)
            val = 1 / val * (SGG * 29.97) / 1000
            Return val
        End Function
        Public Function VaporCp(T As Double, P As Double, SGG As Double, SGO As Double) As Double
            Dim WK, w As Double
            Dim NBP As Double = LiquidNormalBoilingPoint(SGO, 0)
            WK = (1.8 * NBP) ^ (1 / 3) / SGO
            Dim Tc As Double = props1.Tc_LeeKesler(NBP, SGO)
            Dim Pc As Double = props1.Pc_LeeKesler(NBP, SGO)
            w = props1.AcentricFactor_LeeKesler(Tc, Pc, NBP)
            Return PROPS.Cpig_lk(WK, w, T)
        End Function
        Public Function VaporCv(T As Double, P As Double, SGG As Double, SGO As Double) As Double
            Dim MW As Double = VaporMolecularWeight(SGG)
            Return (VaporCp(T, P, SGG, SGO) * MW - 8.314) / MW
        End Function
        Public Function VaporThermalConductivity(T As Double, P As Double, SGG As Double, SGO As Double) As Double
            Dim Tc, Pc, Vc, Zc, w, MW, Cv As Double
            Dim NBP As Double = LiquidNormalBoilingPoint(SGO, 0)
            Tc = props1.Tc_LeeKesler(NBP, SGO)
            Pc = props1.Pc_LeeKesler(NBP, SGO)
            w = props1.AcentricFactor_LeeKesler(Tc, Pc, NBP)
            Zc = PROPS.Zc1(w)
            Vc = 8.314 * Zc * Tc / Pc
            MW = VaporMolecularWeight(SGG)
            Cv = VaporCv(T, P, SGG, SGO)
            Return PROPS.condtg_elyhanley(T, Tc, Vc, Zc, w, MW, Cv)
        End Function
        Public Function VaporViscosity(T As Double, P As Double, SGG As Double) As Double

            Dim Tpc, Ppc, Ppr, Tpr As Double

            Ppc = 677 + 15 * SGG - 37.5 * SGG ^ 2
            Tpc = 168 + 325 * SGG - 12.5 * SGG ^ 2

            Dim Tf, Trank, Ppsia As Double

            Tf = (T - 273.15) * 9 / 5 + 32
            Trank = Tf + 459.67
            Ppsia = P * 0.000145038

            Ppr = Ppsia / Ppc
            Tpr = Trank / Tpc

            Dim mug1, mug1_, mug, A0, A12, A13, A14, A15 As Double
            Dim A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, C As Double

            mug1_ = (0.00001709 - 0.000002062 * SGG) * Tf + 0.008188 - 0.00615 * Log(SGG) / Log(10)

            mug1 = mug1_

            A0 = -2.4621182
            A1 = 2.97054714
            A2 = -0.286264054
            A3 = 0.00805420522
            A4 = 2.80860949
            A5 = -3.49803305
            A6 = 0.36037302
            A7 = -0.0104432413
            A8 = -0.793385684
            A9 = 1.39643306
            A10 = -0.149144925
            A11 = 0.00441015512
            A12 = 0.0839387178
            A13 = -0.186408848
            A14 = 0.0203367881
            A15 = -0.000609579263

            C = A0 + A1 * Ppr + A2 * Ppr ^ 2 + A3 * Ppr ^ 3 + Tpr * (A4 + A5 * Ppr + A6 * Ppr ^ 2 + A7 * Ppr ^ 3) + Tpr ^ 2 * (A8 + A9 * Ppr + A10 * Ppr ^ 2 + A11 * Ppr ^ 3) + Tpr ^ 3 * (A12 + A13 * Ppr + A14 * Ppr ^ 2 + A15 * Ppr ^ 3)

            mug = mug1 / Tpr * Exp(C)

            Return mug * 0.001

        End Function
        Public Function VaporCompressibilityFactor(T As Double, P As Double, SGG As Double) As Double

            Dim Tf, Trank, Ppsia As Double

            Tf = (T - 273.15) * 9 / 5 + 32
            Trank = Tf + 459.67
            Ppsia = P * 0.000145038

            Dim Z, Zant, Tpc, Ppc, rhopr, Ppr, Tpr As Double

            Ppc = 677 + 15 * SGG - 37.5 * SGG ^ 2
            Tpc = 168 + 325 * SGG - 12.5 * SGG ^ 2

            Dim A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, C1, C2, C3 As Double

            A1 = 0.3265
            A2 = -1.07
            A3 = -0.5339
            A4 = 0.01569
            A5 = -0.05165
            A6 = 0.5475
            A7 = -0.7361
            A8 = 0.1844
            A9 = 0.1056
            A10 = 0.6134
            A11 = 0.721

            Ppr = Ppsia / Ppc
            Tpr = Trank / Tpc

            Z = 1

            Dim cnt As Integer = 0

            Do

                rhopr = 0.27 * Ppr / (Z * Tpr)

                C1 = A1 + A2 / Tpr + A3 / Tpr ^ 3 + A4 / Tpr ^ 4 + A5 / Tpr ^ 5
                C2 = A6 + A7 / Tpr + A8 / Tpr ^ 2
                C3 = A7 / Tpr + A8 / Tpr ^ 2

                Zant = Z
                Z = 1 + C1 * rhopr + C2 * rhopr ^ 2 - A9 * C3 * rhopr ^ 5 + A10 * (1 + A11 * rhopr ^ 2) * (rhopr ^ 2 / Tpr ^ 3) * Exp(-A11 * rhopr ^ 2)

                cnt += 1

            Loop Until Abs(Z - Zant) < 0.0001 Or cnt > 1000

            Return Z

        End Function

    End Class

End Namespace


