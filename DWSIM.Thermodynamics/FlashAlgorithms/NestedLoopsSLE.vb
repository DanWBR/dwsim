'    DWSIM Nested Loops Flash Algorithms for Solid-Liquid Equilibria (SLE)
'    Copyright 2013 Daniel Wagner O. de Medeiros
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

    ''' <summary>
    ''' The Flash algorithms in this class are based on the Nested Loops approach to solve equilibrium calculations.
    ''' </summary>
    ''' <remarks></remarks>
    <System.Serializable()> Public Class NestedLoopsSLE

        Inherits FlashAlgorithm

        Dim etol As Double = 0.000001
        Dim itol As Double = 0.000001
        Dim maxit_i As Integer = 100
        Dim maxit_e As Integer = 100
        Dim Hv0, Hvid, Hlid, Hf, Hv, Hl, Hs As Double
        Dim Sv0, Svid, Slid, Sf, Sv, Sl, Ss As Double

        Public Property CompoundProperties As List(Of Interfaces.ICompoundConstantProperties)

        Public Property SolidSolution As Boolean = False

        Sub New()
            MyBase.New()
            Order = 3
        End Sub

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod
            Get
                If SolidSolution Then
                    Return Interfaces.Enums.FlashMethod.Nested_Loops_SLE_SolidSolution
                Else
                    Return Interfaces.Enums.FlashMethod.Nested_Loops_SLE_Eutectic
                End If
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String
            Get
                If GlobalSettings.Settings.CurrentCulture = "pt-BR" Then
                    If SolidSolution Then
                        Return "Algoritmo Flash para sistemas Sólido-Líquido (ESL)"
                    Else
                        Return "Algoritmo Flash para sistemas Sólido-Líquido-Vapor (ESLV)"
                    End If
                Else
                    If SolidSolution Then
                        Return "Flash Algorithm for Solid-Liquid (SLE) systems"
                    Else
                        Return "Flash Algorithm for Vapor-Solid-Liquid (VSLE) systems"
                    End If
                End If
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                If SolidSolution Then
                    Return "Nested Loops (SLE - Solid Solution)"
                Else
                    Return "Nested Loops (SVLE - Eutectic)"
                End If
            End Get
        End Property

        Public Overrides Function Flash_PT(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            CompoundProperties = PP.DW_GetConstantProperties

            If SolidSolution Then
                Return Flash_PT_SS(Vz, P, T, PP, ReuseKI, PrevKi)
            Else
                Return Flash_PT_NL(Vz, P, T, PP, ReuseKI, PrevKi)
            End If

        End Function

        Public Function Flash_PT_SS(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object


            Dim i, n, ecount As Integer
            Dim soma_x, soma_y, soma_s As Double
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, S, Lant, V As Double

            Dim ids As New List(Of String)

            d1 = Date.Now

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            Dim Vn(n) As String, Vx(n), Vy(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), Ki_ant(n), fi(n), Vs(n), Vs_ant(n), activcoeff(n) As Double

            Vn = PP.RET_VNAMES()
            fi = Vz.Clone

            'Calculate Ki`s

            i = 0
            Do
                ids.Add(CompoundProperties(i).Name)
                Vp(i) = PP.AUX_PVAPi(i, T)
                If CompoundProperties(i).TemperatureOfFusion <> 0.0# Then
                    Ki(i) = Exp(-CompoundProperties(i).EnthalpyOfFusionAtTf / (0.00831447 * T) * (1 - T / CompoundProperties(i).TemperatureOfFusion))
                    If Ki(i) = 0.0# Then Ki(i) = 1.0E+20
                Else
                    Ki(i) = 1.0E+20
                End If
                i += 1
            Loop Until i = n + 1

            V = 0.0#
            L = 1.0#
            S = 0.0#

            i = 0
            Do
                If Vz(i) <> 0.0# Then
                    Vx(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * L + 1)
                    If Double.IsNaN(Vx(i)) Then Vx(i) = 0.0#
                    If Ki(i) <> 0 Then Vs(i) = Vx(i) / Ki(i) Else Vs(i) = Vz(i)
                    If Vs(i) < 0 Then Vs(i) = 0
                    If Vx(i) < 0 Then Vx(i) = 0
                Else
                    Vs(i) = 0
                    Vx(i) = 0
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            soma_x = 0
            soma_s = 0
            soma_y = 0.0#
            Do
                soma_x = soma_x + Vx(i)
                soma_s = soma_s + Vs(i)
                i = i + 1
            Loop Until i = n + 1
            i = 0
            Do
                If soma_x > 0.0# Then Vx(i) = Vx(i) / soma_x
                If soma_s > 0.0# Then Vs(i) = Vs(i) / soma_s
                i = i + 1
            Loop Until i = n + 1

            ecount = 0
            Dim convergiu = 0
            Dim F = 0.0#


            Do



                Ki_ant = Ki.Clone

                activcoeff = PP.DW_CalcFugCoeff(Vx, T, P, State.Liquid)

                For i = 0 To n
                    If Double.IsNaN(activcoeff(i)) Then activcoeff(i) = 1.0#
                Next

                For i = 0 To n
                    If Not CompoundProperties(i).IsSalt Then activcoeff(i) = activcoeff(i) * P / Vp(i)
                    If CompoundProperties(i).TemperatureOfFusion <> 0.0# Then
                        Ki(i) = (1 / activcoeff(i)) * Exp(-CompoundProperties(i).EnthalpyOfFusionAtTf / (0.00831447 * T) * (1 - T / CompoundProperties(i).TemperatureOfFusion))
                        If Ki(i) = 0.0# Then Ki(i) = 1.0E+20
                    Else
                        Ki(i) = 1.0E+20
                    End If
                Next

                i = 0
                Do
                    If Vz(i) <> 0 Then
                        Vs_ant(i) = Vs(i)
                        Vx_ant(i) = Vx(i)
                        Vx(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * L + 1)
                        If Double.IsNaN(Vx(i)) Then Vx(i) = 0.0#
                        If Ki(i) <> 0 Then Vs(i) = Vx(i) / Ki(i) Else Vs(i) = Vz(i)
                    Else
                        Vy(i) = 0
                        Vx(i) = 0
                    End If
                    i += 1
                Loop Until i = n + 1

                i = 0
                soma_x = 0
                soma_s = 0
                Do
                    soma_x = soma_x + Vx(i)
                    soma_s = soma_s + Vs(i)
                    i = i + 1
                Loop Until i = n + 1
                i = 0
                Do
                    If soma_x > 0.0# Then Vx(i) = Vx(i) / soma_x
                    If soma_s > 0.0# Then Vs(i) = Vs(i) / soma_s
                    i = i + 1
                Loop Until i = n + 1

                Dim e1 As Double = 0
                Dim e2 As Double = 0
                Dim e3 As Double = 0
                i = 0
                Do
                    e1 = e1 + (Vx(i) - Vx_ant(i))
                    e2 = e2 + (Vs(i) - Vs_ant(i))
                    i = i + 1
                Loop Until i = n + 1

                e3 = (L - Lant)

                If Double.IsNaN(Math.Abs(e1) + Math.Abs(e2)) Then

                    Throw New Exception(Calculator.GetLocalString("PropPack_FlashError"))

                ElseIf Math.Abs(e3) < 0.0000000001 And ecount > 0 Then

                    convergiu = 1

                    Exit Do

                Else

                    Lant = L

                    F = 0.0#
                    Dim dF = 0.0#
                    i = 0
                    Do
                        If Vz(i) > 0 Then
                            F = F + Vz(i) * (Ki(i) - 1) / (1 + L * (Ki(i) - 1))
                            dF = dF - Vz(i) * (Ki(i) - 1) ^ 2 / (1 + L * (Ki(i) - 1)) ^ 2
                        End If
                        i = i + 1
                    Loop Until i = n + 1

                    If Abs(F) < 0.000001 Then Exit Do

                    L = -0.7 * F / dF + L

                End If

                S = 1 - L

                If L > 1 Then
                    L = 1
                    S = 0
                    i = 0
                    Do
                        Vx(i) = Vz(i)
                        i = i + 1
                    Loop Until i = n + 1
                ElseIf L < 0 Then
                    L = 0
                    S = 1
                    i = 0
                    Do
                        Vs(i) = Vz(i)
                        i = i + 1
                    Loop Until i = n + 1
                End If

                ecount += 1

                If Double.IsNaN(L) Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashTPVapFracError"))
                If ecount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2"))

                WriteDebugInfo("PT Flash [NL-SLE]: Iteration #" & ecount & ", LF = " & L)

                If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

            Loop Until convergiu = 1

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PT Flash [NL-SLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms. Error function value: " & F)

out:        Return New Object() {L, V, Vx, Vy, ecount, 0.0#, PP.RET_NullVector, S, Vs, Ki}

        End Function

        Function Flash_SL(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage) As Object

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_SL", Name & " (SLE Flash)", "Pressure-Temperature Solid-Liquid Flash Algorithm Routine", True)

            IObj?.Paragraphs.Add("This routine tries to find the compositions of liquid and solid phases at equilibrium.")

            IObj?.Paragraphs.Add("During the convergence process, solubility is checked for compounds in the liquid phase through the following equilibrium relation:")

            IObj?.Paragraphs.Add("<m>-\ln x_i^L\gamma_i^L= \frac{\Delta h_{m,i}}{RT}\left(1-\frac{T}{T_{m,i}}\right)-\frac{\Delta c_{P,i}(T_{m,i}-T)}{RT}+\frac{\Delta c_{P,i}}{R}\ln \frac{T_m}{T}</m>")

            IObj?.Paragraphs.Add("where <mi>x_i^L</mi> is the compound mole fraction in the liquid phase, <mi>\gamma _i^L</mi> is the activity coefficient of the compound in the liquid phase, <mi>T_{m,i}</mi> is the compound's melting point and <mi>\Delta c_{P,i}</mi> is the heat capacity difference of the compound between liquid and solid states.")

            'This flash is used to calculate the solid/liquid equilibrium at given pressure and temperature
            'Input parameters:  global mole fractions Vz
            'Result Parameters: number of moles in each phase

            Dim MaxError As Double = 0.0000001

            Dim i, n, ecount As Integer
            Dim d1, d2 As Date

            d1 = Date.Now
            n = Vz.Length - 1

            Dim Vx(n), Vs(n), MaxAct(n), MaxX(n), MaxLiquPhase(n), Tf(n), Hf(n), Tc(n), ActCoeff(n), VnL(n), VnS(n), Vp(n) As Double
            Dim L, L_old, SF, SLP As Double
            Dim cpl(n), cps(n), dCp(n) As Double
            Dim Vn(n) As String
            Dim constprop As Interfaces.ICompoundConstantProperties

            Vx = Vz.Clone 'assuming initially only liquids exist
            Tf = PP.RET_VTF 'Fusion temperature
            Hf = PP.RET_VHF 'Enthalpy of fusion
            Tc = PP.RET_VTC 'Critical Temperature

            If Tf.Sum = 0.0 AndAlso PP.ForcedSolids.Count = 0 Then
                'impossible to calculate solids. return liquid solution only.
                L = 1
                L_old = L
                Vx = Vz.Clone
                Vs = PP.RET_NullVector
                GoTo out
            End If

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Critical Temperatures: {0} K", Tc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Fusion Temperatures: {0} K", Tf.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Fusion Enthalpies: {0} kJ/mol", Hf.ToMathArrayString))

            If Vz.MaxY >= 0.999999 Then 'only a single component
                ecount = 0
                For i = 0 To n
                    If Vz(i) >= 0.999999 Then
                        If T > Tf(i) Then
                            'above melting temperature, only liquid
                            L = 1
                            L_old = L
                            Vx = Vz.Clone
                            Vs = PP.RET_NullVector
                            GoTo out
                        Else
                            'below melting temperature, only solid
                            L = 0
                            L_old = L
                            Vs = Vz.Clone
                            Vx = PP.RET_NullVector
                            GoTo out
                        End If
                    End If
                Next
            End If

            Vn = PP.RET_VNAMES()
            For i = 0 To n
                constprop = PP.CurrentMaterialStream.Phases(0).Compounds(Vn(i)).ConstantProperties
                IObj?.SetCurrent
                cpl(i) = PP.AUX_LIQ_Cpi(constprop, Tf(i))
                IObj?.SetCurrent
                cps(i) = PP.AUX_SolidHeatCapacity(constprop, Tf(i))
                'ignoring heat capacity difference due to issues with DWSIM characterization
                dCp(i) = 0.0# '(cpl(i) - cps(i)) * constprop.Molar_Weight
            Next

            'Calculate max activities for solubility of solids
            For i = 0 To n
                MaxAct(i) = Exp(-Hf(i) * 1000 / 8.31446 / T * (1 - T / Tf(i)) - dCp(i) / 8.31446 * ((T - Tf(i)) / T + Log(Tf(i) / T)))
                If Double.IsNaN(MaxAct(i)) Then MaxAct(i) = 1000.0
                IObj?.SetCurrent
                Vp(i) = PP.AUX_PVAPi(i, T)
            Next

            IObj?.Paragraphs.Add(String.Format("<h2>Calculations</h2>"))

            ecount = 0

            L = 1
            Do

                ecount += 1

                IObj?.Paragraphs.Add(String.Format("<h3>Loop {0}</h3>", ecount))

                IObj?.SetCurrent
                If TypeOf PP Is IdealElectrolytePropertyPackage Then
                    ActCoeff = PP.RET_UnitaryVector()
                Else
                    ActCoeff = PP.DW_CalcFugCoeff(Vx, T, P, State.Liquid).MultiplyConstY(P).DivideY(Vp)
                End If
                MaxX = MaxAct.DivideY(ActCoeff)
                For i = 0 To n
                    'check if ion
                    constprop = PP.CurrentMaterialStream.Phases(0).Compounds(Vn(i)).ConstantProperties
                    If constprop.IsIon Then MaxX(i) = 1.0
                    'Supercritical gases are put to liquid phase
                    If T > Tc(i) Then MaxX(i) = 1.0
                    'If compound is in forced solids list, put it in solid phase
                    If PP.ForcedSolids.Contains(Vn(i)) Then MaxX(i) = 0.0
                Next

                IObj?.Paragraphs.Add(String.Format("Activity coefficients: {0}", ActCoeff.ToMathArrayString))
                IObj?.Paragraphs.Add(String.Format("Maximum solubilities (mole fractions): {0}", MaxX.ToMathArrayString))

                MaxLiquPhase = Vz.DivideY(MaxX)
                SF = 0
                For i = 0 To n
                    If MaxLiquPhase(i) > 0.0001 Then
                        SF += MaxX(i)
                    End If
                Next
                If SF < 1 Then
                    'only solid remaining
                    Vx = PP.RET_NullVector
                    Vs = Vz.Clone
                    L = 0
                    L_old = 0
                    Exit Do
                End If

                VnL = PP.RET_NullVector
                VnS = PP.RET_NullVector
                Vx = PP.RET_NullVector

                L_old = L

                For i = 0 To n
                    If Vz(i) > MaxX(i) Then
                        Vx(i) = MaxX(i) 'Component fraction above max solubility. -> fix fraction to max solubility
                    Else
                        VnL(i) = Vz(i) 'Component fraction below max solubility. -> put to liquid completely
                    End If
                Next

                SLP = VnL.SumY 'Sum moles of components in liquid phase
                SF = Vx.SumY 'Sum mole fractions of components fixed by max solubility
                If 1 - SLP < 0.00000001 Then SLP = 1
                L = SLP / (1 - SF)

                If L >= 1 Then
                    'all components are below max solubility, only liquid left
                    Vx = Vz.Clone
                    Vs = PP.RET_NullVector
                    Exit Do
                End If

                'calculate moles in liquid phase of components above max solubility
                For i = 0 To n
                    If Vz(i) > MaxX(i) Then
                        VnL(i) = MaxX(i) * L
                    End If
                    VnS(i) = Vz(i) - VnL(i)
                Next

                If L > 0 And MaxX.SumY > 1 Then
                    Vx = VnL.NormalizeY
                    Vs = VnS.NormalizeY
                Else
                    'only solid remaining
                    Vx = PP.RET_NullVector
                    Vs = Vz.Clone
                    L = 0
                    L_old = 0
                    Exit Do
                End If

                IObj?.Paragraphs.Add(String.Format("Current estimates for liquid phase composition: {0}", Vx.ToMathArrayString))
                IObj?.Paragraphs.Add(String.Format("Current estimates for solid phase composition: {0}", Vs.ToMathArrayString))

                IObj?.Paragraphs.Add(String.Format("Current estimates for liquid phase mole fraction: {0}", L))
                IObj?.Paragraphs.Add(String.Format("Current estimates for solid phase mole fraction: {0}", 1 - L))

                If ecount > maxit_e Then Throw New Exception("SL Flash: max iterations reached")

            Loop Until Abs(L - L_old) < MaxError

out:        d2 = Date.Now

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))

            IObj?.Paragraphs.Add(String.Format("Final liquid phase composition: {0}", Vx.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final solid phase composition: {0}", Vs.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("Final liquid phase mole fraction: {0}", L))
            IObj?.Paragraphs.Add(String.Format("Final solid phase mole fraction: {0}", 1 - L))

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & (d2 - d1).TotalMilliseconds & " ms. Error function value: " & Abs(L - L_old))

            IObj?.Close()

            Return New Object() {L, 1 - L, 0.0#, Vx, Vs, L - L_old, ecount, d2 - d1}

        End Function

        Public Function Flash_PT_NL(ByVal Vz0 As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PT", Name & " (PT Flash)", "Pressure-Temperature Flash Algorithm Routine", True)

            IObj?.Paragraphs.Add("This routine tries to find the compositions of vapor, liquid and solid phases at equilibrium by solving the Rachford-Rice equation using a newton convergence approach.")

            IObj?.Paragraphs.Add("The Rachford-Rice equation is")

            IObj?.Paragraphs.Add("<math>\sum_i\frac{z_i \, (K_i - 1)}{1 + \beta \, (K_i - 1)}= 0</math>")

            IObj?.Paragraphs.Add("where:")

            IObj?.Paragraphs.Add("<math_inline>z_{i}</math_inline> is the mole fraction of component i in the feed liquid (assumed to be known);")
            IObj?.Paragraphs.Add("<math_inline>\beta</math_inline> is the fraction of feed that is vaporised;")
            IObj?.Paragraphs.Add("<math_inline>K_{i}</math_inline> is the equilibrium constant of component i.")

            IObj?.Paragraphs.Add("The equilibrium constants K<sub>i</sub> are in general functions of many parameters, though the most important is arguably temperature; they are defined as:")

            IObj?.Paragraphs.Add("<math>y_i = K_i \, x_i</math>")

            IObj?.Paragraphs.Add("where:")

            IObj?.Paragraphs.Add("<math_inline>x_i</math_inline> is the mole fraction of component i in liquid phase;")
            IObj?.Paragraphs.Add("<math_inline>y_i</math_inline> is the mole fraction of component i in gas phase.")

            IObj?.Paragraphs.Add("Once the Rachford-Rice equation has been solved for <math_inline>\beta</math_inline>, the compositions x<sub>i</sub> and y<sub>i</sub> can be immediately calculated as:")

            IObj?.Paragraphs.Add("<math>x_i =\frac{z_i}{1+\beta(K_i-1)}\\y_i=K_i\,x_i</math>")

            IObj?.Paragraphs.Add("The Rachford - Rice equation can have multiple solutions for <math_inline>\beta</math_inline>, at most one of which guarantees that all <math_inline>x_i</math_inline> and <math_inline>y_i</math_inline> will be positive. In particular, if there is only one <math_inline>\beta</math_inline> for which:")
            IObj?.Paragraphs.Add("<math>\frac{1}{1-K_\text{max}}=\beta_\text{min}<\beta<\beta_\text{max}=\frac{1}{1-K_\text{min}}</math>")
            IObj?.Paragraphs.Add("then that <math_inline>\beta</math_inline> is the solution; if there are multiple  such <math_inline>\beta</math_inline>s, it means that either <math_inline>K_{max}<1</math_inline> or <math_inline>K_{min}>1</math_inline>, indicating respectively that no gas phase can be sustained (and therefore <math_inline>\beta=0</math_inline>) or conversely that no liquid phase can exist (and therefore <math_inline>\beta=1</math_inline>).")

            IObj?.Paragraphs.Add("DWSIM initializes the current calculation with ideal K-values estimated from vapor pressure data for each compound, or by using previously calculated values from an earlier solution.")

            IObj?.Paragraphs.Add("During the convergence process, solubility is checked for compounds in the liquid phase through the following equilibrium relation:")

            IObj?.Paragraphs.Add("<m>-\ln x_i^L\gamma_i^L= \frac{\Delta h_{m,i}}{RT}\left(1-\frac{T}{T_{m,i}}\right)-\frac{\Delta c_{P,i}(T_{m,i}-T)}{RT}+\frac{\Delta c_{P,i}}{R}\ln \frac{T_m}{T}</m>")

            IObj?.Paragraphs.Add("where <mi>x_i^L</mi> is the compound mole fraction in the liquid phase, <mi>\gamma _i^L</mi> is the activity coefficient of the compound in the liquid phase, <mi>T_{m,i}</mi> is the compound's melting point and <mi>\Delta c_{P,i}</mi> is the heat capacity difference of the compound between liquid and solid states.")

            Dim i, n, ecount, gcount As Integer
            Dim Pb, Pd, Pmin, Pmax, Px As Double
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, V, S, Vant As Double
            Dim GL_old, GS_old, GV_old As Double
            Dim GlobalConv As Boolean = False

            d1 = Date.Now

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz0.Length - 1

            Dim Vx(n), Vy(n), Vs(n), Vmix(n), Vx_ant(n), Vy_ant(n), Vp(n), Ki(n), Ki_ant(n), Vz(n) As Double

            Vz = Vz0.Clone

            'Calculate Ki`s
            If Not ReuseKI Then
                i = 0
                Do
                    Vp(i) = PP.AUX_PVAPi(i, T)
                    Ki(i) = Vp(i) / P
                    If Ki(i) = 0.0 Then Ki(i) = 1.0E-20
                    i += 1
                Loop Until i = n + 1
            Else
                For i = 0 To n
                    Vp(i) = PP.AUX_PVAPi(i, T)
                    Ki(i) = PrevKi(i)
                Next
            End If

            'initially put all into liquid phase
            Vx = Vz.Clone
            S = 0.0
            L = 1.0
            V = 0.0

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("<h2>Calculated Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Initial estimate for V: {0}", V))
            IObj?.Paragraphs.Add(String.Format("Initial estimate for L: {0}", L))
            IObj?.Paragraphs.Add(String.Format("Initial estimate for S: {0}", S))

            'do flash calculation iterations
            Do

                GL_old = L
                GV_old = V
                GS_old = S
                gcount += 1

                IObj?.SetCurrent()

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj2, "", "Flash_PT", "PT SVLE Internal Loop Iteration", "Pressure-Temperature Flash Algorithm Convergence Iteration Step", True)

                IObj2?.Paragraphs.Add(String.Format("This is the SVLE convergence loop iteration #{0}. DWSIM will use the current values of y, x and s to calculate fugacity coefficients and update K using the Property Package rigorous models.", ecount))

                If V < 1.0 Then

                    'there is some liquid or solid

                    '================================================
                    '== mix solid and liquid phase ==================
                    '================================================
                    Vmix = Vs.MultiplyConstY(S)
                    Vmix = Vmix.AddY(Vx.MultiplyConstY(L))
                    Vz = Vmix.NormalizeY

                    '================================================
                    '== Do initial SLE flash to precipitate solids ==
                    '================================================
                    Dim SL_Result As Object
                    IObj2?.SetCurrent
                    SL_Result = Flash_SL(Vz, P, T, PP)
                    Vx = SL_Result(3)
                    Vs = SL_Result(4)

                    'calculate global phase fractions
                    L = SL_Result(0) * (1 - V)
                    S = SL_Result(1) * (1 - V)
                End If

                'only solids and/or vapour left
                If L = 0.0 Then GoTo out2

                '================================================
                '== mix vapour and liquid phase =================
                '================================================
                Vmix = Vy.MultiplyConstY(V)
                Vmix = Vmix.AddY(Vx.MultiplyConstY(L))
                Vz = Vmix.NormalizeY

                '================================================
                '== Do VLE flash ================================
                '================================================

                'Estimate V
                If T > MathEx.Common.Max(PP.RET_VTC, Vz) Then
                    Vy = Vz
                    V = 1
                    L = 0
                    GoTo out
                End If

                i = 0
                Px = 0
                Do
                    If Vp(i) <> 0.0# Then Px = Px + (Vz(i) / Vp(i))
                    i = i + 1
                Loop Until i = n + 1
                Px = 1 / Px
                Pmin = Px
                Pmax = SumY(Vz.MultiplyY(Vp))
                Pb = Pmax
                Pd = Pmin

                If Abs(Pb - Pd) / Pb < 0.0000001 Then
                    'one comp only
                    If Px <= P Then
                        L = 1
                        V = 0
                        Vx = Vz
                        GoTo out
                    Else
                        L = 0
                        V = 1
                        Vy = Vz
                        GoTo out
                    End If
                End If

                Dim Vmin, Vmax, g As Double
                Vmin = 1.0#
                Vmax = 0.0#
                For i = 0 To n
                    If (Ki(i) * Vz(i) - 1) / (Ki(i) - 1) < Vmin Then Vmin = (Ki(i) * Vz(i) - 1) / (Ki(i) - 1)
                    If (1 - Vz(i)) / (1 - Ki(i)) > Vmax Then Vmax = (1 - Vz(i)) / (1 - Ki(i))
                Next

                If Vmin < 0.0# Then Vmin = 0.0#
                If Vmin = 1.0# Then Vmin = 0.0#
                If Vmax = 0.0# Then Vmax = 1.0#
                If Vmax > 1.0# Then Vmax = 1.0#

                V = (Vmin + Vmax) / 2

                g = 0.0#
                For i = 0 To n
                    g += Vz(i) * (Ki(i) - 1) / (V + (1 - V) * Ki(i))
                Next

                If g > 0 Then Vmin = V Else Vmax = V

                V = Vmin + (Vmax - Vmin) / 2
                L = 1 - V

                If n = 0 Then
                    If Vp(0) <= P Then
                        L = 1
                        V = 0
                    Else
                        L = 0
                        V = 1
                    End If
                End If

                i = 0
                Do
                    If Vz(i) <> 0 Then
                        Vy(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * V + 1)
                        If Ki(i) <> 0 Then Vx(i) = Vy(i) / Ki(i) Else Vx(i) = Vz(i)
                        If Vy(i) < 0 Then Vy(i) = 0
                        If Vx(i) < 0 Then Vx(i) = 0
                    Else
                        Vy(i) = 0
                        Vx(i) = 0
                    End If
                    i += 1
                Loop Until i = n + 1

                Vy_ant = Vy.Clone
                Vx_ant = Vx.Clone

                Vy = Vz.MultiplyY(Ki).DivideY(Ki.AddConstY(-1).MultiplyConstY(V).AddConstY(1))
                For i = 0 To n
                    If Double.IsNaN(Vy(i)) Then Vy(i) = 0
                Next
                Vx = Vy.DivideY(Ki)

                Vx = Vx.NormalizeY
                Vy = Vy.NormalizeY

                Dim convergiu As Integer = 0
                Dim F, dF, e1, e2, e3 As Double
                ecount = 0

                Do

                    IObj2?.SetCurrent()

                    Dim IObj3 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                    Inspector.Host.CheckAndAdd(IObj3, "", "Flash_PT", "PT Flash VLE Newton Iteration", "Pressure-Temperature Flash Algorithm Convergence Iteration Step")

                    IObj3?.Paragraphs.Add(String.Format("This is the VLE Newton convergence loop iteration #{0}. DWSIM will use the current values of y and x to calculate fugacity coefficients and update K using the Property Package rigorous models.", ecount))

                    IObj3?.SetCurrent()

                    Ki_ant = Ki.Clone
                    Ki = PP.DW_CalcKvalue(Vx, Vy, T, P)

                    IObj3?.Paragraphs.Add(String.Format("K values where updated. Current values: {0}", Ki.ToMathArrayString))

                    Vy_ant = Vy.Clone
                    Vx_ant = Vx.Clone

                    Vy = Vz.MultiplyY(Ki).DivideY(Ki.AddConstY(-1).MultiplyConstY(V).AddConstY(1))
                    For i = 0 To n
                        If Double.IsNaN(Vy(i)) Then Vy(i) = 0
                    Next
                    Vx = Vy.DivideY(Ki)

                    Vx = Vx.NormalizeY
                    Vy = Vy.NormalizeY

                    IObj3?.Paragraphs.Add(String.Format("y values (vapor phase composition) where updated. Current values: {0}", Vy.ToMathArrayString))
                    IObj3?.Paragraphs.Add(String.Format("x values (liquid phase composition) where updated. Current values: {0}", Vx.ToMathArrayString))

                    e1 = Vx.SubtractY(Vx_ant).AbsSumY
                    e2 = Vy.SubtractY(Vy_ant).AbsSumY

                    e3 = (V - Vant)

                    IObj3?.Paragraphs.Add(String.Format("Current Vapor Fraction (<math_inline>\beta</math_inline>) error: {0}", e3))

                    If Double.IsNaN(e1 + e2) Then

                        Throw New Exception(Calculator.GetLocalString("PropPack_FlashError"))

                    ElseIf Math.Abs(e3) < 0.0000000001 And ecount > 0 Then

                        convergiu = 1

                        Exit Do

                    Else

                        Vant = V

                        F = 0.0#
                        dF = 0.0#
                        i = 0
                        Do
                            If Vz(i) > 0 Then
                                F = F + Vz(i) * (Ki(i) - 1) / (1 + V * (Ki(i) - 1))
                                dF = dF - Vz(i) * (Ki(i) - 1) ^ 2 / (1 + V * (Ki(i) - 1)) ^ 2
                            End If
                            i = i + 1
                        Loop Until i = n + 1

                        IObj3?.Paragraphs.Add(String.Format("Current value of the Rachford-Rice error function: {0}", F))

                        If Abs(F) < etol / 100 Then Exit Do

                        Dim dfac = (ecount + 1) * 0.1
                        If dfac > 1.0 Then dfac = 1.0
                        If -F / dF * dfac + Vant > 1.0 Or -F / dF * dfac + Vant < 0.0 Then
                            dfac /= 50
                        End If

                        V = -F / dF * dfac + Vant

                    IObj3?.Paragraphs.Add(String.Format("Updated Vapor Fraction (<math_inline>\beta</math_inline>) value: {0}", V))

                    End If

                    If V < 0.0# Then V = 0.0#
                    If V > 1.0# Then V = 1.0#

                    L = 1 - V

                    ecount += 1

                    If Double.IsNaN(V) Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashTPVapFracError"))
                    If ecount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2"))

                    WriteDebugInfo("PT Flash [NL-SLE]: Iteration #" & ecount & ", VF = " & V)

                    If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

                    IObj3?.Close()

                Loop Until convergiu = 1

out:            'calculate global phase fractions
                L = L * (1 - S)
                V = V * (1 - S)

                If gcount > maxit_e Then Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2"))

out2:           If (Math.Abs(GL_old - L) < 0.0000005) And (Math.Abs(GV_old - V) < 0.0000005) And (Math.Abs(GS_old - S) < 0.0000005) Then GlobalConv = True

                IObj2?.Paragraphs.Add(String.Format("Current estimates for liquid phase composition: {0}", Vx.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("Current estimates for solid phase composition: {0}", Vs.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("Current estimates for vapor phase composition: {0}", Vy.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("Current estimates for liquid phase mole fraction: {0}", L))
                IObj2?.Paragraphs.Add(String.Format("Current estimates for solid phase mole fraction: {0}", S))
                IObj2?.Paragraphs.Add(String.Format("Current estimates for vapor phase mole fraction: {0}", V))

                IObj2?.Close()

            Loop Until GlobalConv

            IObj?.Paragraphs.Add("The algorithm converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            IObj?.Paragraphs.Add(String.Format("Final liquid phase composition: {0}", Vx.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final solid phase composition: {0}", Vs.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final vapor phase composition: {0}", Vy.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("Final liquid phase mole fraction: {0}", L))
            IObj?.Paragraphs.Add(String.Format("Final solid phase mole fraction: {0}", S))
            IObj?.Paragraphs.Add(String.Format("Final vapor phase mole fraction: {0}", V))

            IObj?.Close()

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PT Flash [NL-SLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds)

            Return New Object() {L, V, Vx, Vy, ecount, 0.0#, PP.RET_NullVector, S, Vs}

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim nl = New NestedLoops
            nl.FlashSettings = FlashSettings
            nl.PTFlashFunction = AddressOf Flash_PT
            nl.DisableParallelCalcs = True
            Return nl.Flash_PH(Vz, P, H, Tref, PP, ReuseKI, PrevKi)

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim nl = New NestedLoops
            nl.FlashSettings = FlashSettings
            nl.PTFlashFunction = AddressOf Flash_PT
            nl.DisableParallelCalcs = True
            Return nl.Flash_PS(Vz, P, S, Tref, PP, ReuseKI, PrevKi)

        End Function

        Public Overrides Function Flash_TV(ByVal Vz As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim d1, d2 As Date, dt As TimeSpan
            Dim names = PP.RET_VNAMES
            Dim P, L, S, Vy(), Vx(), Vs(), VxM() As Double
            Dim Lant, Vant, Sant, Pant As Double
            Dim eTotal, eP, eL, eV, eS, eXL, eXV, eXS As Double
            Dim Vxant(), Vyant(), Vsant() As Double
            Dim result As Object = Nothing
            Dim ecount As Integer

            d1 = Date.Now

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            'initialize flash calculations
            Dim nl = New NestedLoops
            nl.FlashSettings = FlashSettings

            Dim nls = New NestedLoopsSLE
            nls.FlashSettings = FlashSettings

            'initialize VLE mixture
            Vx = Vz.Clone
            Vy = PP.RET_NullVector
            Vs = PP.RET_NullVector
            L = 1
            V = 0
            S = 0
            P = Pref

            Do
                ecount += 1

                'save old values
                Lant = L
                Vant = V
                Sant = S
                Vxant = Vx
                Vyant = Vy
                Vsant = Vs
                Pant = P

                'mix vapor and liquid
                VxM = Vx.MultiplyConstY(L).AddY(Vy.MultiplyConstY(V)).NormalizeY

                'first run VLE flash
                result = nl.Flash_TV(VxM, T, V, Pref, PP, ReuseKI, PrevKi)
                'Return New Object() {L, V, Vx, Vy, P, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}
                L = result(0) * (1 - S)
                V = result(1) * (1 - S)
                Vx = result(2)
                Vy = result(3)
                P = result(4)

                'mix solid and liquid
                VxM = Vx.MultiplyConstY(L).AddY(Vs.MultiplyConstY(S)).NormalizeY

                'calculate solid liquid flash with that mixture
                Dim resultS = nls.Flash_SL(VxM, P, T, PP)
                'Return New Object() {L, 1 - L, 0.0#, Vx, Vs, L - L_old, ecount, d2 - d1}
                S = resultS(1) * (1 - V)
                L = resultS(0) * (1 - V)
                Vx = resultS(3)
                Vs = resultS(4)

                'calculate errors
                eL = Abs(L - Lant)
                eV = Abs(V - Vant)
                eS = Abs(S - Sant)
                eP = Abs(P - Pant) / 100
                eXL = Vx.SubtractY(Vxant).AbsSumY
                eXV = Vy.SubtractY(Vyant).AbsSumY
                eXS = Vs.SubtractY(Vsant).AbsSumY

                eTotal = eL + eV + eS + eXL + eXV + eXS + eP

            Loop Until eTotal < etol Or ecount > maxit_e

            d2 = Date.Now
            dt = d2 - d1

            If ecount > maxit_e Then
                Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2"))
                WriteDebugInfo("Error: TV Flash [NL-SLE]: Did not converge in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")
            Else
                WriteDebugInfo("TV Flash [NL-SLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")
            End If

            Return New Object() {L, V, Vx, Vy, P, ecount, PP.RET_NullVector, 0, PP.RET_NullVector, S, Vs}

        End Function

        Public Overrides Function Flash_PV(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim i, n, ecount, gcount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, S, Lf, Vf, Vint, T, Tf, deltaT As Double
            Dim e1 As Double
            Dim AF As Double = 1
            Dim GL_old, GS_old, GV_old As Double

            d1 = Date.Now

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            PP = PP
            Vf = V
            L = 1 - V
            Lf = 1 - Vf
            Tf = T
            Vint = V
            GL_old = L
            GV_old = V
            GS_old = 0

            Dim Vn(n) As String, Vx(n), Vy(n), Vs(n), Vmix(n), Vx_ant(n), Vy_ant(n), Vs_ant(n), Vp(n), Ki(n), fi(n) As Double
            Dim Vt(n), VTc(n), Tmin, Tmax, dFdT, Tsat(n) As Double

            Vn = PP.RET_VNAMES()
            VTc = PP.RET_VTC()
            fi = Vz.Clone

            Tmin = 0.0#
            Tmax = 0.0#

            If Tref = 0.0# Then
                i = 0
                Tref = 0.0#
                Do
                    Tref += 0.8 * Vz(i) * VTc(i)
                    Tmin += 0.1 * Vz(i) * VTc(i)
                    Tmax += 2.0 * Vz(i) * VTc(i)
                    i += 1
                Loop Until i = n + 1
            Else
                Tmin = Tref - 50
                Tmax = Tref + 50
            End If

            T = Tref

            'Calculate Ki`s

            If Not ReuseKI Then
                i = 0
                Do
                    Vp(i) = PP.AUX_PVAPi(Vn(i), T)
                    Ki(i) = Vp(i) / P
                    i += 1
                Loop Until i = n + 1
            Else
                If Not PP.AUX_CheckTrivial(PrevKi) And Not Double.IsNaN(PrevKi(0)) Then
                    For i = 0 To n
                        Vp(i) = PP.AUX_PVAPi(Vn(i), T)
                        Ki(i) = PrevKi(i)
                    Next
                Else
                    i = 0
                    Do
                        Vp(i) = PP.AUX_PVAPi(Vn(i), T)
                        Ki(i) = Vp(i) / P
                        i += 1
                    Loop Until i = n + 1
                End If
            End If

            i = 0
            Do
                If Vz(i) <> 0 Then
                    Vy(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * V + 1)
                    If Double.IsInfinity(Vy(i)) Then Vy(i) = 0.0#
                    Vx(i) = Vy(i) / Ki(i)
                Else
                    Vy(i) = 0
                    Vx(i) = 0
                End If
                i += 1
            Loop Until i = n + 1

            Vy = Vy.NormalizeY()
            Vx = Vx.NormalizeY()

            If PP.AUX_IS_SINGLECOMP(Vz) Then
                WriteDebugInfo("PV Flash [SLE]: Converged in 1 iteration.")
                T = 0
                For i = 0 To n
                    T += Vz(i) * PP.AUX_TSATi(P, i)
                Next
                Return New Object() {L, V, Vx, Vy, T, 0, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}
            End If

            Dim marcador3, marcador2, marcador As Integer
            Dim stmp4_ant, stmp4, Tant, fval As Double
            Dim chk As Boolean = False

            If V = 1.0# Then

                ecount = 0
                Do

                    marcador3 = 0

                    Dim cont_int = 0
                    Do

                        Ki = PP.DW_CalcKvalue(Vx, Vy, T, P)

                        marcador = 0
                        If stmp4_ant <> 0 Then
                            marcador = 1
                        End If
                        stmp4_ant = stmp4

                        If V = 0 Then
                            i = 0
                            stmp4 = 0
                            Do
                                stmp4 = stmp4 + Ki(i) * Vx(i)
                                i = i + 1
                            Loop Until i = n + 1
                        Else
                            i = 0
                            stmp4 = 0
                            Do
                                stmp4 = stmp4 + Vy(i) / Ki(i)
                                i = i + 1
                            Loop Until i = n + 1
                        End If

                        If V = 0 Then
                            Vy_ant = Vy.Clone
                            i = 0
                            Do
                                Vy_ant(i) = Vy(i)
                                Vy(i) = Ki(i) * Vx(i) / stmp4
                                i = i + 1
                            Loop Until i = n + 1
                        Else
                            Vx_ant = Vx.Clone
                            i = 0
                            Do
                                Vx_ant(i) = Vx(i)
                                Vx(i) = (Vy(i) / Ki(i)) / stmp4
                                i = i + 1
                            Loop Until i = n + 1
                        End If

                        marcador2 = 0
                        If marcador = 1 Then
                            If V = 0 Then
                                If Math.Abs(Vy(0) - Vy_ant(0)) < itol Then
                                    marcador2 = 1
                                End If
                            Else
                                If Math.Abs(Vx(0) - Vx_ant(0)) < itol Then
                                    marcador2 = 1
                                End If
                            End If
                        End If

                        cont_int = cont_int + 1

                    Loop Until marcador2 = 1 Or Double.IsNaN(stmp4) Or cont_int > maxit_i

                    Dim K1(n), K2(n), dKdT(n) As Double

                    K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)
                    K2 = PP.DW_CalcKvalue(Vx, Vy, T + 1, P)

                    For i = 0 To n
                        dKdT(i) = (K2(i) - K1(i))
                    Next

                    fval = stmp4 - 1

                    ecount += 1

                    i = 0
                    dFdT = 0
                    Do
                        If V = 0 Then
                            dFdT = dFdT + Vx(i) * dKdT(i)
                        Else
                            dFdT = dFdT - Vy(i) / (Ki(i) ^ 2) * dKdT(i)
                        End If
                        i = i + 1
                    Loop Until i = n + 1

                    Tant = T
                    deltaT = -fval / dFdT

                    If Abs(deltaT) > 0.1 * T Then
                        T = T + Sign(deltaT) * 0.1 * T
                    Else
                        T = T + deltaT
                    End If

                    WriteDebugInfo("PV Flash [SLE]: Iteration #" & ecount & ", T = " & T & ", VF = " & V)

                    If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

                Loop Until Math.Abs(fval) < etol Or Double.IsNaN(T) = True Or ecount > maxit_e

            Else

                Do
                    ecount = 0

                    '================================================
                    '== mix vapour and liquid phase =================
                    '================================================
                    Vmix = Vy.MultiplyConstY(V)
                    Vmix = Vmix.AddY(Vx.MultiplyConstY(L))
                    Vz = Vmix.NormalizeY

                    Do

                        Ki = PP.DW_CalcKvalue(Vx, Vy, T, P)

                        i = 0
                        Do
                            If Vz(i) <> 0 Then
                                Vy_ant(i) = Vy(i)
                                Vx_ant(i) = Vx(i)
                                Vy(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * Vint + 1)
                                Vx(i) = Vy(i) / Ki(i)
                            Else
                                Vy(i) = 0
                                Vx(i) = 0
                                Vy_ant(i) = 0
                                Vx_ant(i) = 0
                            End If
                            i += 1
                        Loop Until i = n + 1

                        Vx = Vx.NormalizeY()
                        Vy = Vy.NormalizeY()

                        If Vint <= 0.5 Then

                            i = 0
                            stmp4 = 0
                            Do
                                stmp4 = stmp4 + Ki(i) * Vx(i)
                                i = i + 1
                            Loop Until i = n + 1

                            Dim K1(n), K2(n), dKdT(n) As Double

                            K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)
                            K2 = PP.DW_CalcKvalue(Vx, Vy, T + 0.01, P)

                            For i = 0 To n
                                dKdT(i) = (K2(i) - K1(i)) / (0.01)
                            Next

                            i = 0
                            dFdT = 0
                            Do
                                dFdT = dFdT + Vx(i) * dKdT(i)
                                i = i + 1
                            Loop Until i = n + 1

                        Else

                            i = 0
                            stmp4 = 0
                            Do
                                stmp4 = stmp4 + Vy(i) / Ki(i)
                                i = i + 1
                            Loop Until i = n + 1

                            Dim K1(n), K2(n), dKdT(n) As Double

                            K1 = PP.DW_CalcKvalue(Vx, Vy, T, P)
                            K2 = PP.DW_CalcKvalue(Vx, Vy, T + 0.01, P)

                            For i = 0 To n
                                dKdT(i) = (K2(i) - K1(i)) / (0.01)
                            Next

                            i = 0
                            dFdT = 0
                            Do
                                dFdT = dFdT - Vy(i) / (Ki(i) ^ 2) * dKdT(i)
                                i = i + 1
                            Loop Until i = n + 1

                        End If

                        ecount += 1

                        fval = stmp4 - 1

                        Tant = T

                        deltaT = -fval / dFdT

                        T = T + deltaT

                        e1 = Vx.SubtractY(Vx_ant).AbsSumY + Vy.SubtractY(Vy_ant).AbsSumY + Math.Abs(T - Tant)

                        WriteDebugInfo("PV Flash [SLE]: Iteration #" & ecount & ", T = " & T & ", VF = " & V)

                        If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

                    Loop Until (Math.Abs(fval) < etol And e1 < etol) Or Double.IsNaN(T) = True Or ecount > maxit_e

                    '================================================
                    '== mix solid and liquid phase ==================
                    '================================================
                    Vmix = Vs.MultiplyConstY(S)
                    Vmix = Vmix.AddY(Vx.MultiplyConstY(L))
                    Vz = Vmix.NormalizeY

                    '================================================
                    '== Do SLE flash to precipitate solids ==========
                    '================================================
                    Vs_ant = Vs.Clone
                    Dim SL_Result As Object
                    SL_Result = Flash_SL(Vz, P, T, PP)
                    Vx = SL_Result(3)
                    Vs = SL_Result(4)

                    '================================================
                    '== Calculate global phase fractions ============
                    '================================================
                    GL_old = L
                    GS_old = S
                    L = SL_Result(0) * (1 - V)
                    S = SL_Result(1) * (1 - V)

                    '===================================================================
                    '== Calculate vapour fraction relative to vapour/liquid ============
                    '===================================================================
                    Vint = V / (1 - S)
                    If Vint > 1 Then
                        'no liquid left, take some solid to vapour phase
                        Vint = 1
                    End If

                    e1 = 1000 * (Abs(GL_old - L) + Abs(GS_old - S))

                    gcount += 1

                Loop Until e1 < etol Or gcount > maxit_e

            End If

            d2 = Date.Now

            dt = d2 - d1

            If ecount > maxit_e Then
                Throw New Exception(Calculator.GetLocalString("PropPack_FlashMaxIt2"))
            End If

            If PP.AUX_CheckTrivial(Ki) Then Throw New Exception("PV Flash [SLE]: Invalid result: converged to the trivial solution (T = " & T & " ).")

            WriteDebugInfo("PV Flash [SLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, S, Vs}

        End Function

        Public Overrides Function Flash_PSF(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            CompoundProperties = PP.DW_GetConstantProperties

            'Pressure/Solid fraction flash

            If SolidSolution Then
                Return Flash_PSF_SS(Vz, P, V, Tref, PP)
            Else
                Return Flash_PSF_E(Vz, P, V, Tref, PP)
            End If

        End Function

        Public Function Flash_PSF_SS(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim i, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim soma_x, soma_s As Double
            Dim L, S, Lf, Sf, T, Tf As Double
            Dim ids As New List(Of String)

            d1 = Date.Now

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            PP = PP
            L = V
            Lf = L
            S = 1 - L
            Lf = 1 - Sf
            Tf = T

            Dim Vn(n) As String, Vx(n), Vs(n), Vx_ant(1), Vs_ant(1), Vp(n), Vp2(n), Ki(n), Ki_ant(n), fi(n), activcoeff(n), activcoeff2(n) As Double
            Dim Vt(n), VTF(n), Tmin, Tmax, dFdT As Double

            Vn = PP.RET_VNAMES()
            VTF = PP.RET_VTF()
            fi = Vz.Clone

            If Tref = 0.0# Then

                i = 0
                Tref = 0
                Do
                    If L = 0 Then
                        Tref = MathEx.Common.Min(VTF)
                    Else
                        Tref += Vz(i) * VTF(i)
                    End If
                    Tmin += 0.1 * Vz(i) * VTF(i)
                    Tmax += 2.0 * Vz(i) * VTF(i)
                    i += 1
                Loop Until i = n + 1

            Else

                Tmin = Tref - 50
                Tmax = Tref + 50

            End If

            T = Tref

            'Calculate Ki`s

            i = 0
            Do
                ids.Add(CompoundProperties(i).Name)
                Vp(i) = PP.AUX_PVAPi(i, T)
                If CompoundProperties(i).TemperatureOfFusion <> 0.0# Then
                    Ki(i) = Exp(-CompoundProperties(i).EnthalpyOfFusionAtTf / (0.00831447 * T) * (1 - T / CompoundProperties(i).TemperatureOfFusion))
                Else
                    Ki(i) = 1.0E+20
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Do
                If Vz(i) <> 0.0# Then
                    Vx(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * L + 1)
                    If Ki(i) <> 0 Then Vs(i) = Vx(i) / Ki(i) Else Vs(i) = Vz(i)
                    If Vs(i) < 0 Then Vs(i) = 0
                    If Vx(i) < 0 Then Vx(i) = 0
                Else
                    Vs(i) = 0
                    Vx(i) = 0
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            soma_x = 0.0#
            soma_s = 0.0#
            Do
                soma_x = soma_x + Vx(i)
                soma_s = soma_s + Vs(i)
                i = i + 1
            Loop Until i = n + 1
            i = 0
            Do
                Vx(i) = Vx(i) / soma_x
                Vs(i) = Vs(i) / soma_s
                i = i + 1
            Loop Until i = n + 1

            Dim marcador3, marcador2, marcador As Integer
            Dim stmp4_ant, stmp4, Tant, fval As Double
            Dim chk As Boolean = False

            ecount = 0
            Do

                marcador3 = 0

                Dim cont_int = 0
                Do

                    Ki_ant = Ki.Clone

                    activcoeff = PP.DW_CalcFugCoeff(Vx, T, P, State.Liquid)

                    For i = 0 To n
                        Vp(i) = PP.AUX_PVAPi(i, T)
                        activcoeff(i) = activcoeff(i) * P / Vp(i)
                        If CompoundProperties(i).TemperatureOfFusion <> 0.0# Then
                            Ki(i) = (1 / activcoeff(i)) * Exp(-CompoundProperties(i).EnthalpyOfFusionAtTf / (0.00831447 * T) * (1 - T / CompoundProperties(i).TemperatureOfFusion))
                        Else
                            Ki(i) = 1.0E+20
                        End If
                    Next

                    marcador = 0
                    If stmp4_ant <> 0 Then
                        marcador = 1
                    End If
                    stmp4_ant = stmp4

                    If L = 0 Then
                        i = 0
                        stmp4 = 0
                        Do
                            stmp4 = stmp4 + Ki(i) * Vs(i)
                            i = i + 1
                        Loop Until i = n + 1
                    Else
                        i = 0
                        stmp4 = 0
                        Do
                            stmp4 = stmp4 + Vx(i) / Ki(i)
                            i = i + 1
                        Loop Until i = n + 1
                    End If

                    If L = 0 Then
                        i = 0
                        Do
                            Vx_ant(i) = Vx(i)
                            Vx(i) = Ki(i) * Vs(i) / stmp4
                            i = i + 1
                        Loop Until i = n + 1
                    Else
                        i = 0
                        Do
                            Vs_ant(i) = Vs(i)
                            Vs(i) = (Vx(i) / Ki(i)) / stmp4
                            i = i + 1
                        Loop Until i = n + 1
                    End If

                    marcador2 = 0
                    If marcador = 1 Then
                        If L = 0 Then
                            If Math.Abs(Vx(0) - Vx_ant(0)) < itol Then
                                marcador2 = 1
                            End If
                        Else
                            If Math.Abs(Vs(0) - Vs_ant(0)) < itol Then
                                marcador2 = 1
                            End If
                        End If
                    End If

                    cont_int = cont_int + 1

                Loop Until marcador2 = 1 Or Double.IsNaN(stmp4) Or cont_int > maxit_i

                Dim K1(n), K2(n), dKdT(n) As Double

                activcoeff = PP.DW_CalcFugCoeff(Vx, T, P, State.Liquid)
                activcoeff2 = PP.DW_CalcFugCoeff(Vx, T + 0.01, P, State.Liquid)

                For i = 0 To n
                    If CompoundProperties(i).TemperatureOfFusion <> 0.0# Then
                        Vp(i) = PP.AUX_PVAPi(i, T)
                        activcoeff(i) = activcoeff(i) * P / Vp(i)
                        K1(i) = (1 / activcoeff(i)) * Exp(-CompoundProperties(i).EnthalpyOfFusionAtTf / (0.00831447 * T) * (1 - T / CompoundProperties(i).TemperatureOfFusion))
                        Vp2(i) = PP.AUX_PVAPi(i, T + 0.01)
                        activcoeff2(i) = activcoeff2(i) * P / Vp2(i)
                        K2(i) = (1 / activcoeff2(i)) * Exp(-CompoundProperties(i).EnthalpyOfFusionAtTf / (0.00831447 * (T + 0.01)) * (1 - (T + 0.01) / CompoundProperties(i).TemperatureOfFusion))
                    Else
                        K1(i) = 1.0E+20
                        K2(i) = 1.0E+20
                    End If
                Next

                For i = 0 To n
                    dKdT(i) = (K2(i) - K1(i)) / 0.01
                Next

                fval = stmp4 - 1

                ecount += 1

                i = 0
                dFdT = 0
                Do
                    If L = 0.0# Then
                        dFdT = dFdT + Vs(i) * dKdT(i)
                    Else
                        dFdT = dFdT - Vx(i) / (Ki(i) ^ 2) * dKdT(i)
                    End If
                    i = i + 1
                Loop Until i = n + 1

                Tant = T
                T = T - fval / dFdT
                'If T < Tmin Then T = Tmin
                'If T > Tmax Then T = Tmax

                WriteDebugInfo("PV Flash [NL-SLE]: Iteration #" & ecount & ", T = " & T & ", LF = " & L)

                If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

            Loop Until Math.Abs(T - Tant) < 0.01 Or Double.IsNaN(T) = True Or ecount > maxit_e Or Double.IsNaN(T) Or Double.IsInfinity(T)

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PSF Flash [NL-SLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {L, V, Vx, PP.RET_NullVector, T, ecount, Ki, 0.0#, PP.RET_NullVector, S, Vs}


        End Function

        Public Function Flash_PSF_E(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim i, n, ecount As Integer
            Dim d1, d2 As Date, dt As TimeSpan
            Dim L, S, Lf, Sf, T, Tf As Double
            Dim ids As New List(Of String)

            d1 = Date.Now

            etol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance).ToDoubleFromInvariant
            maxit_e = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations)
            itol = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance).ToDoubleFromInvariant
            maxit_i = Me.FlashSettings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations)

            n = Vz.Length - 1

            PP = PP
            L = V
            Lf = L
            S = 1 - L
            Lf = 1 - Sf
            Tf = T

            Dim Vn(n) As String, Vx(n), Vs(n), Vx_ant(1), Vs_ant(1), Vp(n), Vp2(n), Ki(n), Ki_ant(n), fi(n), activcoeff(n), activcoeff2(n) As Double
            Dim Vt(n), VTF(n), Tmin, Tmax As Double

            Vn = PP.RET_VNAMES()
            VTF = PP.RET_VTF()
            fi = Vz.Clone

            If Tref = 0.0# Then

                i = 0
                Tref = 0
                Do
                    If L = 0 Then 'L=0
                        Tref = MathEx.Common.Min(VTF)
                    Else
                        Tref += Vz(i) * VTF(i)
                    End If
                    Tmin += 0.1 * Vz(i) * VTF(i)
                    Tmax += 2.0 * Vz(i) * VTF(i)
                    i += 1
                Loop Until i = n + 1

            Else

                Tmin = Tref - 50
                Tmax = Tref + 50

            End If

            T = Tref

            'Calculate Ki`s

            i = 0
            Do
                ids.Add(CompoundProperties(i).Name)
                Vp(i) = PP.AUX_PVAPi(i, T)
                If CompoundProperties(i).TemperatureOfFusion <> 0.0# Then
                    Ki(i) = Exp(-CompoundProperties(i).EnthalpyOfFusionAtTf / (0.00831447 * T) * (1 - T / CompoundProperties(i).TemperatureOfFusion))
                Else
                    Ki(i) = 1.0E+20
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Do
                If Vz(i) <> 0.0# Then
                    Vx(i) = Vz(i) * Ki(i) / ((Ki(i) - 1) * L + 1)
                    If Ki(i) <> 0 Then Vs(i) = Vx(i) / Ki(i) Else Vs(i) = Vz(i)
                    If Vs(i) < 0 Then Vs(i) = 0
                    If Vx(i) < 0 Then Vx(i) = 0
                Else
                    Vs(i) = 0
                    Vx(i) = 0
                End If
                i += 1
            Loop Until i = n + 1

            Vx = Vx.NormalizeY
            Vs = Vx.NormalizeY

            Dim chk As Boolean = False

            Dim result As Object

            If PP.AUX_IS_SINGLECOMP(Vz) Then
                T = 0
                For i = 0 To n
                    T += Vz(i) * Me.CompoundProperties(i).TemperatureOfFusion
                Next
                result = Me.Flash_PT(Vz, P, T, PP)
                Return New Object() {result(0), result(1), result(2), result(3), T, 0, PP.RET_NullVector, 0.0#, PP.RET_NullVector, result(7), result(8)}
            End If

            T = 0
            For i = 0 To n
                T += Vz(i) * Me.CompoundProperties(i).TemperatureOfFusion - 30
                VTF(i) = Me.CompoundProperties(i).TemperatureOfFusion
            Next

            ecount = 0

            Dim SF0, SF1, T0, T1 As Double
            T0 = Common.Min(VTF) * 0.6
            T1 = Common.Max(VTF) + 10

            SF0 = Flash_PT(Vz, P, T0, PP)(7)
            SF1 = Flash_PT(Vz, P, T1, PP)(7)

            Do
                T = (T0 + T1) / 2
                Sf = Flash_PT(Vz, P, T, PP)(7)

                If Sf > V Then
                    T0 = T
                    SF0 = Sf
                Else
                    T1 = T
                    SF1 = Sf
                End If
                ecount += 1
            Loop Until (T1 - T0) <= itol

            result = Me.Flash_PT_NL(Vz, P, T, PP)

            d2 = Date.Now

            dt = d2 - d1

            WriteDebugInfo("PSF Flash [NL-SLE]: Converged in " & ecount & " iterations. Time taken: " & dt.TotalMilliseconds & " ms.")

            Return New Object() {result(0), result(1), result(2), result(3), T, ecount, PP.RET_NullVector, 0.0#, PP.RET_NullVector, result(7), result(8)}

        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

    End Class

End Namespace
