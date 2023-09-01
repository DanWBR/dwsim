'    DWSIM Nested Loops Flash Algorithms for Solid-Liquid Equilibria (SLE)
'    Copyright 2015 Daniel Wagner O. de Medeiros
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

Imports DWSIM.MathOps.MathEx
Imports DWSIM.MathOps.MathEx.Common

Imports System.Threading.Tasks

Imports System.Linq

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    <System.Serializable()> Public Class Seawater

        Inherits FlashAlgorithm

        Dim etol As Double = 0.000001
        Dim itol As Double = 0.000001
        Dim maxit_i As Integer = 100
        Dim maxit_e As Integer = 100
        Dim Hv0, Hvid, Hlid, Hf, Hv, Hl, Hs As Double
        Dim Sv0, Svid, Slid, Sf, Sv, Sl, Ss As Double

        Public Property CompoundProperties As List(Of Interfaces.ICompoundConstantProperties)

        Sub New()
            MyBase.New()
        End Sub

        Public Overrides ReadOnly Property InternalUseOnly As Boolean
            Get
                Return True
            End Get
        End Property

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod
            Get
                Return Interfaces.Enums.FlashMethod.SeaWater
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String
            Get
                If GlobalSettings.Settings.CurrentCulture = "pt-BR" Then
                    Return "Algoritmo Flash para cálculo de equilíbrio de água salgada"
                Else
                    Return "Flash Algorithm for Seawater systems"
                End If
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                Return "Seawater Flash"
            End Get
        End Property

        Public Overrides Function Flash_PT(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Return Flash_PT_Internal(True, Vz, P, T, PP, ReuseKI, PrevKi)

        End Function

        Public Function Flash_PT_Internal(ByVal include_vapor As Boolean, ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            'This flash algorithm is for Electrolye/Salt systems with Water as the single solvent.
            'The vapor and solid phases are considered to be ideal.
            'Chemical equilibria is calculated using the reactions enabled in the default reaction set.

            Dim n As Integer = CompoundProperties.Count - 1
            Dim activcoeff(n) As Double
            Dim i As Integer

            'Vnf = feed molar amounts (considering 1 mol of feed)
            'Vnl = liquid phase molar amounts
            'Vnv = vapor phase molar amounts
            'Vns = solid phase molar amounts
            'Vxl = liquid phase molar fractions
            'Vxv = vapor phase molar fractions
            'Vxs = solid phase molar fractions
            'V, S, L = phase molar amounts (F = 1 = V + S + L)
            Dim Vnf(n), Vnl(n), Vxl(n), Vxl_ant(n), Vns(n), Vxs(n), Vnv(n), Vxv(n), V, S, L, L_ant, Vp(n) As Double
            Dim sumN As Double = 0

            Vnf = Vz.Clone

            Dim wid As Integer = CompoundProperties.IndexOf((From c As Interfaces.ICompoundConstantProperties In CompoundProperties Select c Where c.CAS_Number = "7732-18-5").SingleOrDefault)

            'calculate SLE.

            Dim ids As New List(Of String)
            For i = 0 To n
                ids.Add(CompoundProperties(i).Name)
                Vp(i) = PP.AUX_PVAPi(i, T)
            Next

            Vxl = Vz.Clone

            'initial estimates for L and S.

            L = 0.0#

            'calculate liquid phase activity coefficients.

            Dim ecount As Integer = 0
            Dim errfunc As Double = 0.0#

            'maximum salt solubility on water in kg/kg

            Dim maxsol As Double = 0.0000025676 * T ^ 2 - 0.001321 * T + 0.52555

            'temperature of fusion

            Dim Tfus As Double = DirectCast(PP, SeawaterPropertyPackage).TemperatureOfFusion(Vxl, T)

            Do

                Vp(wid) = DirectCast(PP, SeawaterPropertyPackage).VaporPressure(Vxl, T)

                activcoeff = PP.DW_CalcFugCoeff(Vxl, T, P, State.Liquid)

                For i = 0 To n
                    If Not CompoundProperties(i).IsSalt Then activcoeff(i) = activcoeff(i) * P / PP.AUX_PVAPi(ids(i), T)
                Next

                Dim Vxlmax(n) As Double

                'calculate maximum solubilities for solids/precipitates.

                For i = 0 To n
                    If CompoundProperties(i).Name = "Salt" Then
                        Vxlmax(i) = maxsol / 58.44 * 18 * Vxl(wid)
                    Else
                        If T >= Tfus Then
                            Vxlmax(i) = 1.0#
                        Else
                            Vxlmax(i) = 0.0#
                        End If
                    End If
                Next

                'mass balance.

                Dim hassolids As Boolean = False

                S = 0.0#
                For i = 0 To n
                    If Vnf(i) > Vxlmax(i) Then
                        hassolids = True
                        Vxl(i) = Vxlmax(i)
                        S += Vnf(i) - Vxl(i) * L
                    End If
                Next

                'check for vapors

                If include_vapor Then

                    V = 0.0#
                    For i = 0 To n
                        If P < Vp(i) Then
                            V += Vnf(i)
                            Vxl(i) = 0.0000000001
                            Vnv(i) = Vnf(i)
                        End If
                    Next

                Else

                    V = 0.0#
                    Vnv = PP.RET_NullVector

                End If

                L_ant = L
                If hassolids Then L = 1 - S - V Else L = 1 - V
                If L < 0.0000000001 Then
                    L = 0.0
                    S = S / (S + V)
                    V = 1 - S
                End If

                For i = 0 To n
                    Vns(i) = Vnf(i) - Vxl(i) * L - Vnv(i)
                    Vnl(i) = Vxl(i) * L
                Next

                For i = 0 To n
                    If Sum(Vnl) <> 0.0# Then Vxl(i) = Vnl(i) / Sum(Vnl) Else Vxl(i) = 0.0000000001
                    If Sum(Vns) <> 0.0# Then Vxs(i) = Vns(i) / Sum(Vns) Else Vxs(i) = 0.0000000001
                    If Sum(Vnv) <> 0.0# Then Vxv(i) = Vnv(i) / Sum(Vnv) Else Vxv(i) = 0.0000000001
                Next

                errfunc = Abs(L - L_ant)

                If errfunc <= 0.0000000001 Then Exit Do

                If Double.IsNaN(S) Then Throw New Exception(Calculator.GetLocalString("PP_FlashTPSolidFracError"))
                If ecount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PP_FlashMaxIt2"))

                ecount += 1

                If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

            Loop

            'return flash calculation results.

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PT Flash [Seawater]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms. Error function value: " & errfunc)

out:        Return New Object() {L, V, Vxl, Vxv, ecount, 0.0#, PP.RET_NullVector, S, Vxs}

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim Vn(1) As String, Vx(1), Vy(1), Vx_ant(1), Vy_ant(1), Vp(1), Ki(1), Ki_ant(1), fi(1), Vs(1) As Double
            Dim i, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, V, T, S, Pf As Double

            d1 = Date.Now

            n = Vz.Length - 1

            PP = PP
            Hf = H
            Pf = P

            ReDim Vn(n), Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), fi(n), Vs(n)

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            Dim maxitINT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations)
            Dim maxitEXT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations)
            Dim tolINT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            Dim tolEXT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance).ToDoubleFromInvariant

            Dim Tsup, Tinf

            Tinf = 100
            Tsup = 2000

            Dim bo As New BrentOpt.Brent
            bo.DefineFuncDelegate(AddressOf Herror)
            WriteDebugInfo("PH Flash: Starting calculation for " & Tinf & " <= T <= " & Tsup)

            Dim fx, fx2, dfdx, x1 As Double

            Dim cnt As Integer = 0

            Tref = 300.0#
            x1 = Tref
            Do
                fx = Herror(x1, {P, Vz, PP})
                fx2 = Herror(x1 + 1, {P, Vz, PP})
                If Abs(fx) < etol Then Exit Do
                dfdx = (fx2 - fx)
                x1 = x1 - fx / dfdx
                If x1 < 0 Then GoTo alt
                cnt += 1
            Loop Until cnt > 100 Or Double.IsNaN(x1)
            If Double.IsNaN(x1) Then
alt:            T = bo.BrentOpt(Tinf, Tsup, 100, tolEXT, maxitEXT, {P, Vz, PP})
            Else
                T = x1
            End If

            Dim Hs, Hl, Hv, xl, xv, xs As Double

            Dim wid As Integer = CompoundProperties.IndexOf((From c As Interfaces.ICompoundConstantProperties In CompoundProperties Select c Where c.CAS_Number = "7732-18-5").SingleOrDefault)

            Hs = PP.DW_CalcSolidEnthalpy(T, Vz, CompoundProperties)
            Hl = PP.DW_CalcEnthalpy(Vz, T, P, State.Liquid)
            Hv = PP.DW_CalcEnthalpy(Vz, T, P, State.Vapor)

            Dim Tsat As Double = PP.AUX_TSATi(P, wid)

            Dim tmp As Object() = Nothing

            If T > Tsat Then
                'vapor only
                tmp = Flash_PT(Vz, P, T, PP)
                L = tmp(0)
                V = tmp(1)
                S = tmp(7)
                Vx = tmp(2)
                Vy = tmp(3)
                Vs = tmp(8)
            ElseIf H <= Hs Then
                'solids only.
                L = 0.0#
                V = 0.0#
                S = 1.0#
                Vx = PP.RET_NullVector()
                Vy = PP.RET_NullVector()
                Vs = Vz
            ElseIf H > Hs And H <= Hl Then
                'partial liquefaction.
                xl = (H - Hs) / (Hl - Hs)
                If xl < 0.999 Then
                    xs = 1 - xl
                Else
                    xl = 1.0
                    xs = 0.0
                End If
                tmp = Flash_PT(Vz, P, T, PP)
                L = xl
                V = 0.0#
                S = xs
                Vx = Vz
                Vy = PP.RET_NullVector()
                Vs = PP.RET_NullVector()
                Vs(wid) = 1.0
            ElseIf H > Hl And H <= Hv Then
                'partial vaporization.
                xv = (H - Hl) / (Hv - Hl)
                xl = 1 - xv
                Vz(wid) -= xv
                Vz = Vz.NormalizeY()
                tmp = Flash_PT(Vz, P, T, PP)
                L = tmp(0) * xl
                V = xv
                S = tmp(7) * xl
                Vx = tmp(2)
                Vy = PP.RET_NullVector()
                Vy(wid) = 1.0#
                Vs = tmp(8)
            Else
                'vapor only.
                tmp = Flash_PT(Vz, P, T, PP)
                L = tmp(0)
                V = tmp(1)
                S = tmp(7)
                Vx = tmp(2)
                Vy = tmp(3)
                Vs = tmp(8)
            End If

            If tmp IsNot Nothing Then ecount = tmp(4)

            For i = 0 To n
                Ki(i) = Vy(i) / Vx(i)
            Next

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PH Flash [Seawater]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, S, Vs}

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim doparallel As Boolean = Settings.EnableParallelProcessing

            Dim Vn(1) As String, Vx(1), Vy(1), Vx_ant(1), Vy_ant(1), Vp(1), Ki(1), Ki_ant(1), fi(1), Vs(1) As Double
            Dim i, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, V, Ss, T, Pf As Double

            d1 = Date.Now

            n = Vz.Length - 1

            PP = PP
            Sf = S
            Pf = P

            ReDim Vn(n), Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), fi(n), Vs(n)

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            Dim maxitINT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations)
            Dim maxitEXT As Integer = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations)
            Dim tolINT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            Dim tolEXT As Double = Me.FlashSettings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance).ToDoubleFromInvariant

            Dim Tsup, Tinf ', Ssup, Sinf

            If Tref <> 0 Then
                Tinf = Tref - 200
                Tsup = Tref + 200
            Else
                Tinf = 100
                Tsup = 2000
            End If
            If Tinf < 100 Then Tinf = 100
            Dim bo As New BrentOpt.Brent
            bo.DefineFuncDelegate(AddressOf Serror)
            WriteDebugInfo("PS Flash: Starting calculation for " & Tinf & " <= T <= " & Tsup)

            Dim fx, fx2, dfdx, x1 As Double

            Dim cnt As Integer = 0

            If Tref = 0 Then Tref = 298.15
            x1 = Tref
            Do
                fx = Serror(x1, {P, Vz, PP})
                fx2 = Serror(x1 + 1, {P, Vz, PP})
                If Abs(fx) < etol Then Exit Do
                dfdx = (fx2 - fx)
                x1 = x1 - fx / dfdx
                If x1 < 0 Then GoTo alt
                cnt += 1
            Loop Until cnt > 50 Or Double.IsNaN(x1)
            If Double.IsNaN(x1) Then
alt:            T = bo.BrentOpt(Tinf, Tsup, 10, tolEXT, maxitEXT, {P, Vz, PP})
            Else
                T = x1
            End If

            Dim Ss1, Sl, Sv, xl, xv, xs As Double

            Dim wid As Integer = CompoundProperties.IndexOf((From c As Interfaces.ICompoundConstantProperties In CompoundProperties Select c Where c.CAS_Number = "7732-18-5").SingleOrDefault)

            Ss1 = PP.DW_CalcSolidEnthalpy(T, Vz, CompoundProperties) / T
            Sl = PP.DW_CalcEntropy(Vz, T, P, State.Liquid)
            Sv = PP.DW_CalcEntropy(Vz, T, P, State.Vapor)

            Dim Tsat As Double = PP.AUX_TSATi(P, wid)

            Dim tmp As Object() = Nothing

            If T > Tsat Then
                'vapor only
                tmp = Flash_PT(Vz, P, T, PP)
                L = tmp(0)
                V = tmp(1)
                Ss = tmp(7)
                Vx = tmp(2)
                Vy = tmp(3)
                Vs = tmp(8)
            ElseIf S <= Ss1 Then
                'solids only.
                tmp = Flash_PT(Vz, P, T, PP)
                L = 0.0#
                V = 0.0#
                Ss = 1.0#
                Vx = PP.RET_NullVector()
                Vy = PP.RET_NullVector()
                Vs = Vz
            ElseIf S > Ss1 And S <= Sl Then
                'partial liquefaction.
                xl = (S - Ss) / (Sl - Ss1)
                xs = 1 - xl
                tmp = Flash_PT(Vz, P, T, PP)
                L = xl
                V = 0.0#
                Ss = xs
                Vx = Vz
                Vy = PP.RET_NullVector()
                Vs = Vz
            ElseIf S > Sl And S <= Sv Then
                'partial vaporization.
                xv = (S - Sl) / (Sv - Sl)
                xl = 1 - xv
                Vz(wid) -= xv
                Vz = Vz.NormalizeY()
                tmp = Flash_PT(Vz, P, T, PP)
                L = tmp(0) * xl
                V = xv
                Ss = tmp(7) * xl
                Vx = tmp(2)
                Vy = PP.RET_NullVector()
                Vy(wid) = 1.0#
                Vs = tmp(8)
            Else
                'vapor only.
                tmp = Flash_PT(Vz, P, T, PP)
                L = tmp(0)
                V = tmp(1)
                Ss = tmp(7)
                Vx = tmp(2)
                Vy = tmp(3)
                Vs = tmp(8)
            End If

            ecount = tmp(4)

            For i = 0 To n
                Ki(i) = Vy(i) / Vx(i)
            Next

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PS Flash [Seawater]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, Ss, Vs}

        End Function

        Function OBJ_FUNC_PH_FLASH(ByVal T As Double, ByVal H As Double, ByVal P As Double, ByVal Vz As Object, ByVal pp As PropertyPackage) As Object

            Dim tmp As Object
            tmp = Me.Flash_PT(Vz, P, T, pp)
            Dim L, V, S, Vx(), Vy(), Vs(), _Hv, _Hl, _Hs As Double

            Dim n = Vz.Length - 1

            L = tmp(0)
            V = tmp(1)
            S = tmp(7)
            Vx = tmp(2)
            Vy = tmp(3)
            Vs = tmp(8)

            _Hv = 0
            _Hl = 0
            _Hs = 0

            Dim mmg, mml, mms As Double
            If V > 0 Then _Hv = pp.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
            If L > 0 Then _Hl = pp.DW_CalcEnthalpy(Vx, T, P, State.Liquid)
            If S > 0 Then _Hs = pp.DW_CalcSolidEnthalpy(T, Vs, CompoundProperties)
            mmg = pp.AUX_MMM(Vy)
            mml = pp.AUX_MMM(Vx)
            mms = pp.AUX_MMM(Vs)

            Dim herr As Double = Hf - (mmg * V / (mmg * V + mml * L + mms * S)) * _Hv - (mml * L / (mmg * V + mml * L + mms * S)) * _Hl - (mms * S / (mmg * V + mml * L + mms * S)) * _Hs
            OBJ_FUNC_PH_FLASH = herr

            WriteDebugInfo("PH Flash [Seawater]: Current T = " & T & ", Current H Error = " & herr)

        End Function

        Function OBJ_FUNC_PS_FLASH(ByVal T As Double, ByVal S As Double, ByVal P As Double, ByVal Vz As Object, ByVal pp As PropertyPackage) As Object

            Dim tmp As Object
            tmp = Me.Flash_PT(Vz, P, T, pp)
            Dim L, V, Ssf, Vx(), Vy(), Vs(), _Sv, _Sl, _Ss As Double

            Dim n = Vz.Length - 1

            L = tmp(0)
            V = tmp(1)
            Ssf = tmp(7)
            Vx = tmp(2)
            Vy = tmp(3)
            Vs = tmp(8)

            _Sv = 0
            _Sl = 0
            _Ss = 0
            Dim mmg, mml, mms As Double

            If V > 0 Then _Sv = pp.DW_CalcEntropy(Vy, T, P, State.Vapor)
            If L > 0 Then _Sl = pp.DW_CalcEntropy(Vx, T, P, State.Liquid)
            If Ssf > 0 Then _Ss = pp.DW_CalcSolidEnthalpy(T, Vs, CompoundProperties) / (T - 298.15)
            mmg = pp.AUX_MMM(Vy)
            mml = pp.AUX_MMM(Vx)
            mms = pp.AUX_MMM(Vs)

            Dim serr As Double = Sf - (mmg * V / (mmg * V + mml * L + mms * Ssf)) * _Sv - (mml * L / (mmg * V + mml * L + mms * Ssf)) * _Sl - (mms * Ssf / (mmg * V + mml * L + mms * Ssf)) * _Ss
            OBJ_FUNC_PS_FLASH = serr

            WriteDebugInfo("PS Flash [Seawater]: Current T = " & T & ", Current S Error = " & serr)

        End Function

        Function Herror(ByVal Tt As Double, ByVal otherargs As Object) As Double
            Return OBJ_FUNC_PH_FLASH(Tt, Hf, otherargs(0), otherargs(1), otherargs(2))
        End Function

        Function Serror(ByVal Tt As Double, ByVal otherargs As Object) As Double
            Return OBJ_FUNC_PS_FLASH(Tt, Sf, otherargs(0), otherargs(1), otherargs(2))
        End Function

        Public Overrides Function Flash_TV(ByVal Vz As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            Dim Vx(n), Vy(n), Vs(n), xv, xl, L, S, P As Double
            Dim wid As Integer = CompoundProperties.IndexOf((From c As Interfaces.ICompoundConstantProperties In CompoundProperties Select c Where c.CAS_Number = "7732-18-5").SingleOrDefault)

            xv = V
            xl = 1 - V

            P = DirectCast(PP, SeawaterPropertyPackage).VaporPressure(Vz, T)

            Dim tmp As Object() = Nothing

            If xv < Vz(wid) Then
                Vz(wid) -= xv
                Vz = Vz.NormalizeY()
                tmp = Flash_PT_Internal(False, Vz, P, T, PP)
            Else
                Throw New InvalidOperationException("Vapor fraction higher than water mole fraction in the mixture.")
            End If
            L = (tmp(0) + tmp(1)) * xl
            If L < 0.000001 Then L = 0.0#
            V = xv
            S = tmp(7) * xl
            If S < 0.000001 Then S = 0.0#
            For i = 0 To n
                Vx(i) = (tmp(2)(i) * tmp(0) + tmp(3)(i) * tmp(1)) / L
            Next
            Vx = Vx.NormalizeY()
            Vy = PP.RET_NullVector()
            Vy(wid) = 1.0#
            Vs = tmp(8)

            ecount = tmp(4)

            d2 = Date.Now

            dt = d2 - d1

            If ecount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2"))

            WriteDebugInfo("TV Flash [Seawater]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {L, V, Vx, Vy, P, ecount, Vy.DivideY(Vx), 0.0#, PP.RET_NullVector, S, Vs}

        End Function

        Public Overrides Function Flash_PV(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            Dim Vx(n), Vy(n), Vs(n), xv, xl, L, S, T As Double
            Dim wid As Integer = CompoundProperties.IndexOf((From c As Interfaces.ICompoundConstantProperties In CompoundProperties Select c Where c.CAS_Number = "7732-18-5").SingleOrDefault)

            xv = V
            xl = 1 - V

            T = DirectCast(PP, SeawaterPropertyPackage).SaturationTemperature(Vz, P)

            Dim tmp As Object() = Nothing

            If xv < Vz(wid) Then
                Vz(wid) -= xv
                Vz = Vz.NormalizeY()
                tmp = Flash_PT_Internal(False, Vz, P, T, PP)
            Else
                Throw New InvalidOperationException("Vapor fraction higher than water mole fraction in the mixture.")
            End If
            L = (tmp(0) + tmp(1)) * xl
            If L < 0.000001 Then L = 0.0#
            V = xv
            S = tmp(7) * xl
            If S < 0.000001 Then S = 0.0#
            For i = 0 To n
                Vx(i) = (tmp(2)(i) * tmp(0) + tmp(3)(i) * tmp(1)) / L
            Next
            Vx = Vx.NormalizeY()
            Vy = PP.RET_NullVector()
            Vy(wid) = 1.0#
            Vs = tmp(8)

            ecount = tmp(4)

            d2 = Date.Now

            dt = d2 - d1

            If ecount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2"))

            WriteDebugInfo("PV Flash [Seawater]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {L, V, Vx, Vy, T, ecount, Vy.DivideY(Vx), 0.0#, PP.RET_NullVector, S, Vs}

        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property
    End Class

End Namespace

