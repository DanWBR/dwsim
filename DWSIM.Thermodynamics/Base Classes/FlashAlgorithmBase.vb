'    Flash Algorithm Abstract Base Class
'    Copyright 2010-2015 Daniel Wagner O. de Medeiros
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
Imports System.Threading.Tasks
Imports DWSIM.Thermodynamics.PropertyPackages.ThermoPlugs
Imports DWSIM.Interfaces.Enums.FlashSetting
Imports System.Linq

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    ''' <summary>
    ''' This is the base class for the flash algorithms.
    ''' </summary>
    ''' <remarks></remarks>
    <System.Serializable()> Public MustInherit Class FlashAlgorithm

        Public Property FlashSettings As New Dictionary(Of Interfaces.Enums.FlashSetting, String)

        Public Property StabSearchSeverity As Integer = 0
        Public Property StabSearchCompIDs As String() = New String() {}

        Private _P As Double, _Vz, _Vx1est, _Vx2est As Double(), _pp As PropertyPackage

        Sub New()

            FlashSettings.Add(ThreePhaseFlashStabTestSeverity, "0")
            FlashSettings.Add(ThreePhaseFlashStabTestCompIds, "")
            FlashSettings.Add(CalculateBubbleAndDewPoints, "False")
            FlashSettings.Add(ValidateEquilibriumCalc, "False")
            FlashSettings.Add(UsePhaseIdentificationAlgorithm, "False")
            FlashSettings.Add(ValidationGibbsTolerance, "1")

        End Sub

        Public Sub WriteDebugInfo(text As String)

            Calculator.WriteToConsole(text, 1)

        End Sub

        ''' <summary>
        ''' Calculates Phase Equilibria for a given mixture at specified conditions.
        ''' </summary>
        ''' <param name="spec1">Flash state specification 1</param>
        ''' <param name="spec2">Flash state specification 2</param>
        ''' <param name="val1">Value of the first flash state specification (P in Pa, T in K, H in kJ/kg, S in kJ/[kg.K], VAP/SF in mole fraction from 0 to 1)</param>
        ''' <param name="val2">Value of the second flash state specification (P in Pa, T in K, H in kJ/kg, S in kJ/[kg.K], VAP/SF in mole fraction from 0 to 1)</param>
        ''' <param name="pp">Property Package instance</param>
        ''' <param name="mixmolefrac">Vector of mixture mole fractions</param>
        ''' <param name="initialKval">Vector containing initial estimates for the K-values (set to 'Nothing' (VB) or 'null' (C#) if none).</param>
        ''' <param name="initialestimate">Initial estimate for Temperature (K) or Pressure (Pa), whichever will be calculated</param>
        ''' <returns>A FlashCalculationResult instance with the results of the calculations</returns>
        ''' <remarks>This function must be used instead of the older type-specific flash functions.
        ''' Check if the 'ResultException' property of the result object is nothing/null before proceeding.</remarks>
        Public Function CalculateEquilibrium(spec1 As FlashSpec, spec2 As FlashSpec,
                                            val1 As Double, val2 As Double,
                                            pp As PropertyPackage,
                                            mixmolefrac As Double(),
                                            initialKval As Double(),
                                            initialestimate As Double) As FlashCalculationResult

            Dim constprops As List(Of Interfaces.ICompoundConstantProperties) = pp.DW_GetConstantProperties()

            Dim d1, d2 As Date

            Dim result As Object = Nothing
            Dim calcresult As New FlashCalculationResult(constprops)

            With calcresult
                .MixtureMoleAmounts = New List(Of Double)(mixmolefrac)
                .FlashAlgorithmType = Me.GetType.ToString
                .FlashSpecification1 = spec1
                .FlashSpecification2 = spec2
            End With

            Dim useestimates = False

            If Not initialKval Is Nothing Then useestimates = True

            d1 = Date.Now

            Try
                If spec1 = FlashSpec.P And spec2 = FlashSpec.T Then
                    'PT = {L1, V, Vx1, Vy, ecount, L2, Vx2, S, Vs}
                    result = Flash_PT(mixmolefrac, val1, val2, pp, useestimates, initialKval)
                    With calcresult
                        .Kvalues = New List(Of Double)(DirectCast(result(3), Double()).DivideY(DirectCast(result(2), Double())))
                        .VaporPhaseMoleAmounts = New List(Of Double)(DirectCast(result(3), Double()).MultiplyConstY(Convert.ToDouble(result(1))))
                        .LiquidPhase1MoleAmounts = New List(Of Double)(DirectCast(result(2), Double()).MultiplyConstY(Convert.ToDouble(result(0))))
                        .LiquidPhase2MoleAmounts = New List(Of Double)(DirectCast(result(6), Double()).MultiplyConstY(Convert.ToDouble(result(5))))
                        .SolidPhaseMoleAmounts = New List(Of Double)(DirectCast(result(8), Double()).MultiplyConstY(Convert.ToDouble(result(7))))
                        .IterationsTaken = Convert.ToInt32(result(4))
                        .CalculatedEnthalpy = CalculateMixtureEnthalpy(val2, val1, .GetLiquidPhase1MoleFraction, .GetLiquidPhase2MoleFraction, .GetVaporPhaseMoleFraction, .GetSolidPhaseMoleFraction,
                                                                       .GetLiquidPhase1MoleFractions, .GetLiquidPhase2MoleFractions, .GetVaporPhaseMoleFractions, .GetSolidPhaseMoleFractions,
                                                                       pp)
                        .CalculatedEntropy = CalculateMixtureEntropy(val2, val1, .GetLiquidPhase1MoleFraction, .GetLiquidPhase2MoleFraction, .GetVaporPhaseMoleFraction, .GetSolidPhaseMoleFraction,
                                                                       .GetLiquidPhase1MoleFractions, .GetLiquidPhase2MoleFractions, .GetVaporPhaseMoleFractions, .GetSolidPhaseMoleFractions,
                                                                       pp)
                    End With
                ElseIf spec1 = FlashSpec.T And spec2 = FlashSpec.P Then
                    'PT = {L1, V, Vx1, Vy, ecount, L2, Vx2, S, Vs}
                    result = Flash_PT(mixmolefrac, val2, val1, pp, useestimates, initialKval)
                    With calcresult
                        .Kvalues = New List(Of Double)(DirectCast(result(3), Double()).DivideY(DirectCast(result(2), Double())))
                        .VaporPhaseMoleAmounts = New List(Of Double)(DirectCast(result(3), Double()).MultiplyConstY(Convert.ToDouble(result(1))))
                        .LiquidPhase1MoleAmounts = New List(Of Double)(DirectCast(result(2), Double()).MultiplyConstY(Convert.ToDouble(result(0))))
                        .LiquidPhase2MoleAmounts = New List(Of Double)(DirectCast(result(6), Double()).MultiplyConstY(Convert.ToDouble(result(5))))
                        .SolidPhaseMoleAmounts = New List(Of Double)(DirectCast(result(8), Double()).MultiplyConstY(Convert.ToDouble(result(7))))
                        .IterationsTaken = Convert.ToInt32(result(4))
                        .CalculatedEnthalpy = CalculateMixtureEnthalpy(val1, val2, .GetLiquidPhase1MoleFraction, .GetLiquidPhase2MoleFraction, .GetVaporPhaseMoleFraction, .GetSolidPhaseMoleFraction,
                                                        .GetLiquidPhase1MoleFractions, .GetLiquidPhase2MoleFractions, .GetVaporPhaseMoleFractions, .GetSolidPhaseMoleFractions,
                                                        pp)
                        .CalculatedEntropy = CalculateMixtureEntropy(val1, val2, .GetLiquidPhase1MoleFraction, .GetLiquidPhase2MoleFraction, .GetVaporPhaseMoleFraction, .GetSolidPhaseMoleFraction,
                                                                       .GetLiquidPhase1MoleFractions, .GetLiquidPhase2MoleFractions, .GetVaporPhaseMoleFractions, .GetSolidPhaseMoleFractions,
                                                                       pp)
                    End With
                ElseIf spec1 = FlashSpec.P And spec2 = FlashSpec.H Then
                    'PH, PS, PV {L1, V, Vx1, Vy, T, ecount, Ki1, L2, Vx2, S, Vs}
                    result = Flash_PH(mixmolefrac, val1, val2, initialestimate, pp, useestimates, initialKval)
                    With calcresult
                        .Kvalues = New List(Of Double)(DirectCast(result(3), Double()).DivideY(DirectCast(result(2), Double())))
                        .VaporPhaseMoleAmounts = New List(Of Double)(DirectCast(result(3), Double()).MultiplyConstY(Convert.ToDouble(result(1))))
                        .LiquidPhase1MoleAmounts = New List(Of Double)(DirectCast(result(2), Double()).MultiplyConstY(Convert.ToDouble(result(0))))
                        .LiquidPhase2MoleAmounts = New List(Of Double)(DirectCast(result(8), Double()).MultiplyConstY(Convert.ToDouble(result(7))))
                        .SolidPhaseMoleAmounts = New List(Of Double)(DirectCast(result(10), Double()).MultiplyConstY(Convert.ToDouble(result(9))))
                        .CalculatedTemperature = Convert.ToDouble(result(4))
                        .IterationsTaken = Convert.ToInt32(result(5))
                        .CalculatedEnthalpy = CalculateMixtureEnthalpy(.CalculatedTemperature.GetValueOrDefault, val1, .GetLiquidPhase1MoleFraction, .GetLiquidPhase2MoleFraction,
                                                                       .GetVaporPhaseMoleFraction, .GetSolidPhaseMoleFraction, .GetLiquidPhase1MoleFractions, .GetLiquidPhase2MoleFractions,
                                                                       .GetVaporPhaseMoleFractions, .GetSolidPhaseMoleFractions, pp)
                        .CalculatedEntropy = CalculateMixtureEntropy(.CalculatedTemperature.GetValueOrDefault, val1, .GetLiquidPhase1MoleFraction, .GetLiquidPhase2MoleFraction,
                                                                     .GetVaporPhaseMoleFraction, .GetSolidPhaseMoleFraction, .GetLiquidPhase1MoleFractions, .GetLiquidPhase2MoleFractions,
                                                                     .GetVaporPhaseMoleFractions, .GetSolidPhaseMoleFractions, pp)
                    End With
                ElseIf spec1 = FlashSpec.P And spec2 = FlashSpec.S Then
                    'PH, PS, PV {L1, V, Vx1, Vy, T, ecount, Ki1, L2, Vx2, S, Vs}
                    result = Flash_PS(mixmolefrac, val1, val2, initialestimate, pp, useestimates, initialKval)
                    With calcresult
                        .Kvalues = New List(Of Double)(DirectCast(result(3), Double()).DivideY(DirectCast(result(2), Double())))
                        .VaporPhaseMoleAmounts = New List(Of Double)(DirectCast(result(3), Double()).MultiplyConstY(Convert.ToDouble(result(1))))
                        .LiquidPhase1MoleAmounts = New List(Of Double)(DirectCast(result(2), Double()).MultiplyConstY(Convert.ToDouble(result(0))))
                        .LiquidPhase2MoleAmounts = New List(Of Double)(DirectCast(result(8), Double()).MultiplyConstY(Convert.ToDouble(result(7))))
                        .SolidPhaseMoleAmounts = New List(Of Double)(DirectCast(result(10), Double()).MultiplyConstY(Convert.ToDouble(result(9))))
                        .CalculatedTemperature = Convert.ToDouble(result(4))
                        .IterationsTaken = Convert.ToInt32(result(5))
                        .CalculatedEnthalpy = CalculateMixtureEnthalpy(.CalculatedTemperature.GetValueOrDefault, val1, .GetLiquidPhase1MoleFraction, .GetLiquidPhase2MoleFraction,
                                                                        .GetVaporPhaseMoleFraction, .GetSolidPhaseMoleFraction, .GetLiquidPhase1MoleFractions, .GetLiquidPhase2MoleFractions,
                                                                        .GetVaporPhaseMoleFractions, .GetSolidPhaseMoleFractions, pp)
                        .CalculatedEntropy = CalculateMixtureEntropy(.CalculatedTemperature.GetValueOrDefault, val1, .GetLiquidPhase1MoleFraction, .GetLiquidPhase2MoleFraction,
                                                                     .GetVaporPhaseMoleFraction, .GetSolidPhaseMoleFraction, .GetLiquidPhase1MoleFractions, .GetLiquidPhase2MoleFractions,
                                                                     .GetVaporPhaseMoleFractions, .GetSolidPhaseMoleFractions, pp)
                    End With
                ElseIf spec1 = FlashSpec.P And spec2 = FlashSpec.VAP Then
                    'PH, PS, PV {L1, V, Vx1, Vy, T, ecount, Ki1, L2, Vx2, S, Vs}
                    result = Flash_PV(mixmolefrac, val1, val2, initialestimate, pp, useestimates, initialKval)
                    With calcresult
                        .Kvalues = New List(Of Double)(DirectCast(result(3), Double()).DivideY(DirectCast(result(2), Double())))
                        .VaporPhaseMoleAmounts = New List(Of Double)(DirectCast(result(3), Double()).MultiplyConstY(Convert.ToDouble(result(1))))
                        .LiquidPhase1MoleAmounts = New List(Of Double)(DirectCast(result(2), Double()).MultiplyConstY(Convert.ToDouble(result(0))))
                        .LiquidPhase2MoleAmounts = New List(Of Double)(DirectCast(result(8), Double()).MultiplyConstY(Convert.ToDouble(result(7))))
                        .SolidPhaseMoleAmounts = New List(Of Double)(DirectCast(result(10), Double()).MultiplyConstY(Convert.ToDouble(result(9))))
                        .CalculatedTemperature = Convert.ToDouble(result(4))
                        .IterationsTaken = Convert.ToInt32(result(5))
                        .CalculatedEnthalpy = CalculateMixtureEnthalpy(.CalculatedTemperature.GetValueOrDefault, val1, .GetLiquidPhase1MoleFraction, .GetLiquidPhase2MoleFraction,
                                                                             .GetVaporPhaseMoleFraction, .GetSolidPhaseMoleFraction, .GetLiquidPhase1MoleFractions, .GetLiquidPhase2MoleFractions,
                                                                             .GetVaporPhaseMoleFractions, .GetSolidPhaseMoleFractions, pp)
                        .CalculatedEntropy = CalculateMixtureEntropy(.CalculatedTemperature.GetValueOrDefault, val1, .GetLiquidPhase1MoleFraction, .GetLiquidPhase2MoleFraction,
                                                                     .GetVaporPhaseMoleFraction, .GetSolidPhaseMoleFraction, .GetLiquidPhase1MoleFractions, .GetLiquidPhase2MoleFractions,
                                                                     .GetVaporPhaseMoleFractions, .GetSolidPhaseMoleFractions, pp)
                    End With
                ElseIf spec1 = FlashSpec.T And spec2 = FlashSpec.VAP Then
                    'TV {L1, V, Vx1, Vy, P, ecount, Ki1, L2, Vx2, S, Vs}
                    result = Flash_TV(mixmolefrac, val1, val2, initialestimate, pp, useestimates, initialKval)
                    With calcresult
                        .Kvalues = New List(Of Double)(DirectCast(result(3), Double()).DivideY(DirectCast(result(2), Double())))
                        .VaporPhaseMoleAmounts = New List(Of Double)(DirectCast(result(3), Double()).MultiplyConstY(Convert.ToDouble(result(1))))
                        .LiquidPhase1MoleAmounts = New List(Of Double)(DirectCast(result(2), Double()).MultiplyConstY(Convert.ToDouble(result(0))))
                        .LiquidPhase2MoleAmounts = New List(Of Double)(DirectCast(result(8), Double()).MultiplyConstY(Convert.ToDouble(result(7))))
                        .SolidPhaseMoleAmounts = New List(Of Double)(DirectCast(result(10), Double()).MultiplyConstY(Convert.ToDouble(result(9))))
                        .CalculatedPressure = Convert.ToDouble(result(4))
                        .IterationsTaken = Convert.ToInt32(result(5))
                        .CalculatedEnthalpy = CalculateMixtureEnthalpy(val1, .CalculatedPressure.GetValueOrDefault, .GetLiquidPhase1MoleFraction, .GetLiquidPhase2MoleFraction,
                                                                            .GetVaporPhaseMoleFraction, .GetSolidPhaseMoleFraction, .GetLiquidPhase1MoleFractions, .GetLiquidPhase2MoleFractions,
                                                                            .GetVaporPhaseMoleFractions, .GetSolidPhaseMoleFractions, pp)
                        .CalculatedEntropy = CalculateMixtureEntropy(val1, .CalculatedPressure.GetValueOrDefault, .GetLiquidPhase1MoleFraction, .GetLiquidPhase2MoleFraction,
                                                                     .GetVaporPhaseMoleFraction, .GetSolidPhaseMoleFraction, .GetLiquidPhase1MoleFractions, .GetLiquidPhase2MoleFractions,
                                                                     .GetVaporPhaseMoleFractions, .GetSolidPhaseMoleFractions, pp)
                    End With
                ElseIf spec1 = FlashSpec.P And spec2 = FlashSpec.SF Then
                    'PH, PS, PV {L1, V, Vx1, Vy, T, ecount, Ki1, L2, Vx2, S, Vs}
                    result = Flash_PSF(mixmolefrac, val1, val2, initialestimate, pp, useestimates, initialKval)
                    With calcresult
                        .Kvalues = New List(Of Double)(DirectCast(result(3), Double()).DivideY(DirectCast(result(2), Double())))
                        .VaporPhaseMoleAmounts = New List(Of Double)(DirectCast(result(3), Double()).MultiplyConstY(Convert.ToDouble(result(1))))
                        .LiquidPhase1MoleAmounts = New List(Of Double)(DirectCast(result(2), Double()).MultiplyConstY(Convert.ToDouble(result(0))))
                        .LiquidPhase2MoleAmounts = New List(Of Double)(DirectCast(result(8), Double()).MultiplyConstY(Convert.ToDouble(result(7))))
                        .SolidPhaseMoleAmounts = New List(Of Double)(DirectCast(result(10), Double()).MultiplyConstY(Convert.ToDouble(result(9))))
                        .CalculatedTemperature = Convert.ToDouble(result(4))
                        .IterationsTaken = Convert.ToInt32(result(5))
                    End With
                ElseIf spec1 = FlashSpec.T And spec2 = FlashSpec.SF Then
                    Throw New NotImplementedException("Flash specification set not supported.")
                Else
                    Throw New NotImplementedException("Flash specification set not supported.")
                End If

                d2 = Date.Now

                calcresult.TimeTaken = (d2 - d1)

            Catch ex As Exception

                calcresult.ResultException = ex

            End Try

            Return calcresult

        End Function

        Function CalculateMixtureEnthalpy(ByVal T As Double, ByVal P As Double, ByVal L As Double, ByVal L2 As Double, ByVal V As Double, ByVal S As Double,
                                          ByVal Vx As Double(), ByVal Vx2 As Double(), ByVal Vy As Double(), ByVal Vs As Double(), ByVal pp As PropertyPackage) As Double

            Dim _Hv, _Hl, _Hl2, _Hs As Double

            Dim n As Integer = UBound(Vx)

            _Hv = 0
            _Hl = 0
            _Hl2 = 0
            _Hs = 0

            Dim mmg, mml, mml2, mms As Double

            If V > 0.0# And Vy.Sum > 0.0# Then _Hv = pp.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
            If L > 0.0# And Vx.Sum > 0.0# Then _Hl = pp.DW_CalcEnthalpy(Vx, T, P, State.Liquid)
            If L2 > 0.0# And Vx2.Sum > 0.0# Then _Hl2 = pp.DW_CalcEnthalpy(Vx2, T, P, State.Liquid)
            If S > 0.0# And Vs.Sum > 0.0# Then _Hs = pp.DW_CalcSolidEnthalpy(T, Vs, pp.DW_GetConstantProperties)

            If V > 0.0# And Vy.Sum > 0.0# Then mmg = pp.AUX_MMM(Vy)
            If L > 0.0# And Vx.Sum > 0.0# Then mml = pp.AUX_MMM(Vx)
            If L2 > 0.0# And Vx2.Sum > 0.0# Then mml2 = pp.AUX_MMM(Vx2)
            If S > 0.0# And Vs.Sum > 0.0# Then mms = pp.AUX_MMM(Vs)

            Return (mmg * V / (mmg * V + mml * L + mml2 * L2 + mms * S)) * _Hv +
                (mml * L / (mmg * V + mml * L + mml2 * L2 + mms * S)) * _Hl +
                (mml2 * L2 / (mmg * V + mml * L + mml2 * L2 + mms * S)) * _Hl2 +
                (mms * S / (mmg * V + mml * L + mml2 * L2 + mms * S)) * _Hs

        End Function

        Function CalculateMixtureEntropy(ByVal T As Double, ByVal P As Double, ByVal L As Double, ByVal L2 As Double, ByVal V As Double, ByVal S As Double,
                                  ByVal Vx As Double(), ByVal Vx2 As Double(), ByVal Vy As Double(), ByVal Vs As Double(), ByVal pp As PropertyPackage) As Double

            Dim _Sv, _Sl, _Sl2 As Double

            Dim n As Integer = UBound(Vx)

            _Sv = 0
            _Sl = 0
            _Sl2 = 0

            Dim mmg, mml, mml2, mms As Double

            If V > 0.0# And Vy.Sum > 0.0# Then _Sv = pp.DW_CalcEntropy(Vy, T, P, State.Vapor)
            If L > 0.0# And Vx.Sum > 0.0# Then _Sl = pp.DW_CalcEntropy(Vx, T, P, State.Liquid)
            If L2 > 0.0# And Vx2.Sum > 0.0# Then _Sl2 = pp.DW_CalcEntropy(Vx2, T, P, State.Liquid)

            If V > 0.0# And Vy.Sum > 0.0# Then mmg = pp.AUX_MMM(Vy)
            If L > 0.0# And Vx.Sum > 0.0# Then mml = pp.AUX_MMM(Vx)
            If L2 > 0.0# And Vx2.Sum > 0.0# Then mml2 = pp.AUX_MMM(Vx2)

            Return (mmg * V / (mmg * V + mml * L + mml2 * L2 + mms * S)) * _Sv +
                (mml * L / (mmg * V + mml * L + mml2 * L2 + mms * S)) * _Sl +
                (mml2 * L2 / (mmg * V + mml * L + mml2 * L2 + mms * S)) * _Sl2

        End Function

#Region "Generic Functions"

        Public Overridable Function Flash_PSF(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object
            Throw New Exception(Calculator.GetLocalString("PropPack_FlashPSFError"))
            Return Nothing
        End Function

        Public MustOverride Function Flash_PT(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

        Public MustOverride Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

        Public MustOverride Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

        Public MustOverride Function Flash_PV(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

        Public MustOverride Function Flash_TV(ByVal Vz As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

#End Region

#Region "Auxiliary Functions"

        Public Function BubbleTemperature_LLE(ByVal Vz As Double(), ByVal Vx1est As Double(), ByVal Vx2est As Double(), ByVal P As Double, ByVal Tmin As Double, ByVal Tmax As Double, ByVal PP As PropertyPackages.PropertyPackage) As Double

            _P = P
            _pp = PP
            _Vz = Vz
            _Vx1est = Vx1est
            _Vx2est = Vx2est

            Dim T, err As Double

            Dim bm As New MathEx.BrentOpt.BrentMinimize
            bm.DefineFuncDelegate(AddressOf BubbleTemperature_LLEPerror)

            err = bm.brentoptimize(Tmin, Tmax, 0.0001, T)

            err = BubbleTemperature_LLEPerror(T)

            Return T

        End Function

        Private Function BubbleTemperature_LLEPerror(ByVal x As Double) As Double

            Dim n As Integer = UBound(_Vz)

            Dim Vp(n), fi1(n), fi2(n), act1(n), act2(n), Vx1(n), Vx2(n) As Double

            Dim result As Object = New SimpleLLE() With {.UseInitialEstimatesForPhase1 = True, .UseInitialEstimatesForPhase2 = True,
                                                          .InitialEstimatesForPhase1 = _Vx1est, .InitialEstimatesForPhase2 = _Vx2est}.Flash_PT(_Vz, _P, x, _pp)

            'Dim result As Object = New GibbsMinimization3P() With {.ForceTwoPhaseOnly = False, .StabSearchSeverity = 0, .StabSearchCompIDs = _pp.RET_VNAMES}.Flash_PT(_Vz, _P, x, _pp)

            Vx1 = result(2)
            Vx2 = result(6)
            fi1 = _pp.DW_CalcFugCoeff(Vx1, x, _P, State.Liquid)
            fi2 = _pp.DW_CalcFugCoeff(Vx2, x, _P, State.Liquid)

            Dim i As Integer

            For i = 0 To n
                Vp(i) = _pp.AUX_PVAPi(i, x)
                act1(i) = _P / Vp(i) * fi1(i)
                act2(i) = _P / Vp(i) * fi2(i)
            Next

            Dim err As Double = _P
            For i = 0 To n
                err -= Vx2(i) * act2(i) * Vp(i)
            Next

            Return Math.Abs(err)

        End Function

        Public Function BubblePressure_LLE(ByVal Vz As Double(), ByVal Vx1est As Double(), ByVal Vx2est As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage) As Double

            Dim n As Integer = UBound(_Vz)

            Dim Vp(n), fi1(n), fi2(n), act1(n), act2(n), Vx1(n), Vx2(n) As Double

            Dim result As Object = New GibbsMinimization3P() With {.ForceTwoPhaseOnly = False,
                                                                   .StabSearchCompIDs = _pp.RET_VNAMES,
                                                                   .StabSearchSeverity = 0}.Flash_PT(_Vz, P, T, PP)

            Vx1 = result(2)
            Vx2 = result(6)
            fi1 = _pp.DW_CalcFugCoeff(Vx1, T, P, State.Liquid)
            fi2 = _pp.DW_CalcFugCoeff(Vx2, T, P, State.Liquid)

            Dim i As Integer

            For i = 0 To n
                Vp(i) = _pp.AUX_PVAPi(i, T)
                act1(i) = P / Vp(i) * fi1(i)
                act2(i) = P / Vp(i) * fi2(i)
            Next

            _P = 0.0#
            For i = 0 To n
                _P += Vx2(i) * act2(i) * Vp(i)
            Next

            Return _P

        End Function

#End Region

#Region "Liquid Phase Stability Check"

        Public Function StabTest(ByVal T As Double, ByVal P As Double, ByVal Vz As Array, ByVal pp As PropertyPackage, Optional ByVal VzArray(,) As Double = Nothing, Optional ByVal searchseverity As Integer = 0)

            WriteDebugInfo("Starting Liquid Phase Stability Test @ T = " & T & " K & P = " & P & " Pa for the following trial phases:")

            Dim i, j, c, n, o, l, nt, maxits As Integer
            n = UBound(Vz)
            nt = UBound(VzArray, 1)

            Dim Y, K As Double(,), tol As Double
            Dim fcv(n), fcl(n) As Double

            Select Case searchseverity
                Case 0
                    ReDim Y(nt, n)
                    tol = 0.0001
                    maxits = 100
                Case 1
                    ReDim Y(nt + 1, n)
                    tol = 0.00001
                    maxits = 100
                Case Else
                    ReDim Y(nt + 2, n)
                    tol = 0.000001
                    maxits = 200
            End Select

            For i = 0 To nt
                Dim text As String = "{"
                For j = 0 To n
                    Y(i, j) = VzArray(i, j)
                    text += VzArray(i, j).ToString & vbTab
                Next
                text.TrimEnd(New Char() {vbTab})
                text += "}"
                WriteDebugInfo(text)
            Next

            ReDim K(0, n)

            Dim m As Integer = UBound(Y, 1)

            Dim h(n), lnfi_z(n), Y_ant(m, n) As Double

            Dim gl, gv As Double

            If Settings.EnableParallelProcessing Then
                
                Dim task1 = Task.Factory.StartNew(Sub()
                                                      fcv = pp.DW_CalcFugCoeff(Vz, T, P, State.Vapor)
                                                  End Sub,
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                     Settings.AppTaskScheduler)
                Dim task2 = Task.Factory.StartNew(Sub()
                                                      fcl = pp.DW_CalcFugCoeff(Vz, T, P, State.Liquid)
                                                  End Sub,
                                                  Settings.TaskCancellationTokenSource.Token,
                                                  TaskCreationOptions.None,
                                                 Settings.AppTaskScheduler)
                Task.WaitAll(task1, task2)
                
            Else
                fcv = pp.DW_CalcFugCoeff(Vz, T, P, State.Vapor)
                fcl = pp.DW_CalcFugCoeff(Vz, T, P, State.Liquid)
            End If

            gv = 0.0#
            gl = 0.0#
            For i = 0 To n
                If Vz(i) <> 0.0# Then gv += Vz(i) * Log(fcv(i) * Vz(i))
                If Vz(i) <> 0.0# Then gl += Vz(i) * Log(fcl(i) * Vz(i))
            Next

            If gl <= gv Then
                lnfi_z = fcl
            Else
                lnfi_z = fcv
            End If

            For i = 0 To n
                lnfi_z(i) = Log(lnfi_z(i))
            Next

            i = 0
            Do
                h(i) = Log(Vz(i)) + lnfi_z(i)
                i = i + 1
            Loop Until i = n + 1

            If Not VzArray Is Nothing Then
                If searchseverity = 1 Then
                    Dim sum0(n) As Double
                    i = 0
                    Do
                        sum0(i) = 0
                        j = 0
                        Do
                            sum0(i) += VzArray(j, i)
                            j = j + 1
                        Loop Until j = UBound(VzArray, 1) + 1
                        i = i + 1
                    Loop Until i = n + 1
                    i = 0
                    Do
                        If Vz(i) <> 0.0# Then
                            Y(nt + 1, i) = sum0(i) / UBound(VzArray, 1)
                        Else
                            Y(n + 1, i) = 0.0#
                        End If
                        i = i + 1
                    Loop Until i = n + 1
                End If
                If searchseverity = 2 Then
                    Dim sum0(n) As Double
                    i = 0
                    Do
                        sum0(i) = 0
                        j = 0
                        Do
                            sum0(i) += VzArray(j, i)
                            j = j + 1
                        Loop Until j = UBound(VzArray, 1) + 1
                        i = i + 1
                    Loop Until i = n + 1
                    i = 0
                    Do
                        If Vz(i) <> 0.0# Then
                            Y(nt + 1, i) = sum0(i) / UBound(VzArray, 1)
                            Y(nt + 2, i) = Exp(h(i))
                        Else
                            Y(n + 1, i) = 0.0#
                        End If
                        i = i + 1
                    Loop Until i = n + 1
                End If
            Else
                i = 0
                Do
                    If Vz(i) <> 0.0# Then
                        Y(n + 1, i) = Exp(h(i))
                    Else
                        Y(n + 1, i) = 0.0#
                    End If
                    i = i + 1
                Loop Until i = n + 1
            End If

            Dim lnfi(m, n), beta(m), r(m), r_ant(m) As Double
            Dim currcomp(n) As Double
            Dim dgdY(m, n), g_(m), tmpfug(n), dY(m, n), sum3 As Double
            Dim excidx As New ArrayList
            Dim finish As Boolean = True

            c = 0
            Do

                'start stability test for each one of the initial estimate vectors
                i = 0
                Do
                    If Not excidx.Contains(i) Then
                        j = 0
                        sum3 = 0
                        Do
                            If Y(i, j) > 0 Then sum3 += Y(i, j)
                            j = j + 1
                        Loop Until j = n + 1
                        j = 0
                        Do
                            If Y(i, j) > 0 Then currcomp(j) = Y(i, j) / sum3 Else currcomp(j) = 0
                            j = j + 1
                        Loop Until j = n + 1

                        If Settings.EnableParallelProcessing Then
                            
                            Dim task1 = Task.Factory.StartNew(Sub()
                                                                  fcv = pp.DW_CalcFugCoeff(currcomp, T, P, State.Vapor)
                                                              End Sub,
                                                                  Settings.TaskCancellationTokenSource.Token,
                                                                  TaskCreationOptions.None,
                                                                 Settings.AppTaskScheduler)
                            Dim task2 = Task.Factory.StartNew(Sub()
                                                                  fcl = pp.DW_CalcFugCoeff(currcomp, T, P, State.Liquid)
                                                              End Sub,
                                                              Settings.TaskCancellationTokenSource.Token,
                                                              TaskCreationOptions.None,
                                                             Settings.AppTaskScheduler)
                            Task.WaitAll(task1, task2)
                            
                        Else
                            fcv = pp.DW_CalcFugCoeff(currcomp, T, P, State.Vapor)
                            fcl = pp.DW_CalcFugCoeff(currcomp, T, P, State.Liquid)
                        End If

                        gv = 0.0#
                        gl = 0.0#
                        For j = 0 To n
                            If currcomp(j) <> 0.0# Then gv += currcomp(j) * Log(fcv(j) * currcomp(j))
                            If currcomp(j) <> 0.0# Then gl += currcomp(j) * Log(fcl(j) * currcomp(j))
                        Next

                        If gl <= gv Then
                            tmpfug = fcl
                        Else
                            tmpfug = fcv
                        End If

                        j = 0
                        Do
                            lnfi(i, j) = Log(tmpfug(j))
                            j = j + 1
                        Loop Until j = n + 1
                        j = 0
                        Do
                            dgdY(i, j) = Log(Y(i, j)) + lnfi(i, j) - h(j)
                            j = j + 1
                        Loop Until j = n + 1
                        j = 0
                        beta(i) = 0
                        Do
                            beta(i) += (Y(i, j) - Vz(j)) * dgdY(i, j)
                            j = j + 1
                        Loop Until j = n + 1
                        g_(i) = 1
                        j = 0
                        Do
                            g_(i) += Y(i, j) * (Log(Y(i, j)) + lnfi(i, j) - h(j) - 1)
                            j = j + 1
                        Loop Until j = n + 1
                        If i > 0 Then r_ant(i) = r(i) Else r_ant(i) = 0
                        r(i) = 2 * g_(i) / beta(i)
                    End If
                    i = i + 1
                Loop Until i = m + 1

                i = 0
                Do
                    If (Abs(g_(i)) < 0.0000000001 And r(i) > 0.9 And r(i) < 1.1) Then
                        If Not excidx.Contains(i) Then excidx.Add(i)
                        'ElseIf c > 4 And r(i) > r_ant(i) Then
                        '    If Not excidx.Contains(i) Then excidx.Add(i)
                    End If
                    i = i + 1
                Loop Until i = m + 1

                i = 0
                Do
                    If Not excidx.Contains(i) Then
                        j = 0
                        Do
                            Y_ant(i, j) = Y(i, j)
                            Y(i, j) = Exp(h(j) - lnfi(i, j))
                            dY(i, j) = Y(i, j) - Y_ant(i, j)
                            If Y(i, j) < 0 Then Y(i, j) = 0
                            j = j + 1
                        Loop Until j = n + 1
                    End If
                    i = i + 1
                Loop Until i = m + 1

                'check convergence

                finish = True
                i = 0
                Do
                    If Not excidx.Contains(i) Then
                        j = 0
                        Do
                            If Abs(dY(i, j)) > tol Then finish = False
                            j = j + 1
                        Loop Until j = n + 1
                    End If
                    i = i + 1
                Loop Until i = m + 1

                c = c + 1

                If c > maxits Then Throw New Exception("Liquid Phase Stability Test: Maximum Iterations Reached.")

            Loop Until finish = True

            ' search for trivial solutions

            Dim sum As Double
            i = 0
            Do
                If Not excidx.Contains(i) Then
                    j = 0
                    sum = 0
                    Do
                        sum += Abs(Y(i, j) - Vz(j))
                        j = j + 1
                    Loop Until j = n + 1
                    If sum < 0.001 Then
                        If Not excidx.Contains(i) Then excidx.Add(i)
                    End If
                End If
                i = i + 1
            Loop Until i = m + 1

            ' search for trivial solutions

            Dim sum5 As Double
            i = 0
            Do
                If Not excidx.Contains(i) Then
                    j = 0
                    sum5 = 0
                    Do
                        sum5 += Y(i, j)
                        j = j + 1
                    Loop Until j = n + 1
                    If Abs(sum5 - 1) < 0.001 Then
                        'phase is stable
                        If Not excidx.Contains(i) Then excidx.Add(i)
                    End If
                End If
                i = i + 1
            Loop Until i = m + 1

            ' join similar solutions

            Dim similar As Boolean

            i = 0
            Do
                If Not excidx.Contains(i) Then
                    o = 0
                    Do
                        If Not excidx.Contains(o) And i <> o Then
                            similar = True
                            j = 0
                            Do
                                If Abs(Y(i, j) - Y(o, j)) > 0.00001 Then
                                    similar = False
                                End If
                                j = j + 1
                            Loop Until j = n + 1
                            If similar Then
                                excidx.Add(o)
                                Exit Do
                            End If
                        End If
                        o = o + 1
                    Loop Until o = m + 1
                End If
                i = i + 1
            Loop Until i = m + 1

            l = excidx.Count
            Dim sum2 As Double
            Dim isStable As Boolean

            If m + 1 - l > 0 Then

                'the phase is unstable

                isStable = False

                'normalize initial estimates

                WriteDebugInfo("Liquid Phase Stability Test finished. Phase is NOT stable. Initial estimates for incipient liquid phase composition:")

                For i = 0 To nt
                    For j = 0 To n
                        Y(i, j) = VzArray(i, j)
                    Next
                Next

                Dim inest(m - l, n) As Double
                i = 0
                l = 0
                Do
                    If Not excidx.Contains(i) Then
                        Dim text As String = "{"
                        j = 0
                        sum2 = 0
                        Do
                            sum2 += Y(i, j)
                            j = j + 1
                        Loop Until j = n + 1
                        j = 0
                        Do
                            inest(l, j) = Y(i, j) / sum2
                            text += inest(l, j).ToString & vbTab
                            j = j + 1
                        Loop Until j = n + 1
                        text.TrimEnd(New Char() {vbTab})
                        text += "}"
                        WriteDebugInfo(text)
                        l = l + 1
                    End If
                    i = i + 1
                Loop Until i = m + 1
                Return New Object() {isStable, inest}
            Else

                'the phase is stable

                WriteDebugInfo("Liquid Phase Stability Test finished. Phase is stable.")

                isStable = True
                Return New Object() {isStable, Nothing}
            End If

        End Function

#End Region

#Region "Phase Type Verification"

        ''' <summary>
        ''' This algorithm returns the state of a fluid given its composition and system conditions.
        ''' </summary>
        ''' <param name="Vx">Vector of mole fractions</param>
        ''' <param name="P">Pressure in Pa</param>
        ''' <param name="T">Temperature in K</param>
        ''' <param name="pp">Property Package instance</param>
        ''' <param name="eos">Equation of State: 'PR' or 'SRK'.</param>
        ''' <returns>A string indicating the phase: 'V' or 'L'.</returns>
        ''' <remarks>This algorithm is based on the method present in the following paper:
        ''' G. Venkatarathnam, L.R. Oellrich, Identification of the phase of a fluid using partial derivatives of pressure, volume, and temperature
        ''' without reference to saturation properties: Applications in phase equilibria calculations, Fluid Phase Equilibria, Volume 301, Issue 2, 
        ''' 25 February 2011, Pages 225-233, ISSN 0378-3812, http://dx.doi.org/10.1016/j.fluid.2010.12.001.
        ''' (http://www.sciencedirect.com/science/article/pii/S0378381210005935)
        ''' Keywords: Phase identification; Multiphase equilibria; Process simulators</remarks>
        Public Shared Function IdentifyPhase(Vx As Double(), P As Double, T As Double, pp As PropertyPackage, ByVal eos As String) As String

            Dim PIP, Tinv As Double, newphase As String, tmp As Double()

            tmp = CalcPIP(Vx, P, T, pp, eos)

            PIP = tmp(0)
            Tinv = tmp(1)

            If Tinv < 500 Then
                Dim fx, fx2, dfdx As Double
                Dim i As Integer = 0
                Do
                    If Settings.EnableParallelProcessing Then
                        
                        Dim task1 As Task = New Task(Sub()
                                                         fx = 1 - CalcPIP(Vx, P, Tinv, pp, eos)(0)
                                                     End Sub)
                        Dim task2 As Task = New Task(Sub()
                                                         fx2 = 1 - CalcPIP(Vx, P, Tinv - 1, pp, eos)(0)
                                                     End Sub)
                        task1.Start()
                        task2.Start()
                        Task.WaitAll(task1, task2)
                        
                    Else
                        fx = 1 - CalcPIP(Vx, P, Tinv, pp, eos)(0)
                        fx2 = 1 - CalcPIP(Vx, P, Tinv - 1, pp, eos)(0)
                    End If
                    dfdx = (fx - fx2)
                    Tinv = Tinv - fx / dfdx
                    i += 1
                Loop Until Math.Abs(fx) < 0.000001 Or i = 25
            End If

            If Double.IsNaN(Tinv) Or Double.IsInfinity(Tinv) Then Tinv = 2000

            If T > Tinv Then
                If PIP > 1 Then newphase = "V" Else newphase = "L"
            Else
                If PIP > 1 Then newphase = "L" Else newphase = "V"
            End If

            Return newphase

        End Function

        ''' <summary>
        ''' This algorithm returns the Phase Identification (PI) parameter for a fluid given its composition and system conditions.
        ''' </summary>
        ''' <param name="Vx">Vector of mole fractions</param>
        ''' <param name="P">Pressure in Pa</param>
        ''' <param name="T">Temperature in K</param>
        ''' <param name="pp">Property Package instance</param>
        ''' <param name="eos">Equation of State: 'PR' or 'SRK'.</param>
        ''' <returns>A string indicating the phase: 'V' or 'L'.</returns>
        ''' <remarks>This algorithm is based on the method present in the following paper:
        ''' G. Venkatarathnam, L.R. Oellrich, Identification of the phase of a fluid using partial derivatives of pressure, volume, and temperature
        ''' without reference to saturation properties: Applications in phase equilibria calculations, Fluid Phase Equilibria, Volume 301, Issue 2, 
        ''' 25 February 2011, Pages 225-233, ISSN 0378-3812, http://dx.doi.org/10.1016/j.fluid.2010.12.001.
        ''' (http://www.sciencedirect.com/science/article/pii/S0378381210005935)
        ''' Keywords: Phase identification; Multiphase equilibria; Process simulators</remarks>
        Private Shared Function CalcPIP(Vx As Double(), P As Double, T As Double, pp As PropertyPackage, ByVal eos As String) As Double()

            Dim g1, g2, g3, g4, g5, g6, t1, t2, v, a, b, dadT, R As Double, tmp As Double()

            If eos = "SRK" Then
                t1 = 1
                t2 = 0
                tmp = ThermoPlugs.SRK.ReturnParameters(T, P, Vx, pp.RET_VKij, pp.RET_VTC, pp.RET_VPC, pp.RET_VW)
            Else
                t1 = 1 + 2 ^ 0.5
                t2 = 1 - 2 ^ 0.5
                tmp = ThermoPlugs.PR.ReturnParameters(T, P, Vx, pp.RET_VKij, pp.RET_VTC, pp.RET_VPC, pp.RET_VW)
            End If

            a = tmp(0)
            b = tmp(1)
            v = tmp(2)
            dadT = tmp(3)

            g1 = 1 / (v - b)
            g2 = 1 / (v + t1 * b)
            g3 = 1 / (v + t2 * b)
            g4 = g2 + g3
            g5 = dadT
            g6 = g2 * g3

            R = 8.314

            Dim d2PdvdT, dPdT, d2Pdv2, dPdv As Double

            d2PdvdT = -R * g1 ^ 2 + g4 * g5 * g6
            dPdT = R * g1 - g5 * g6
            d2Pdv2 = 2 * R * T * g1 ^ 3 - 2 * a * g6 * (g2 ^ 2 + g6 + g3 ^ 2)
            dPdv = -R * T * g1 ^ 2 + a * g4 * g6

            Dim PIP As Double

            PIP = v * (d2PdvdT / dPdT - d2Pdv2 / dPdv)

            Dim Tinv As Double

            Tinv = 2 * a * (v - b) ^ 2 / (R * b * v ^ 2)

            Return New Double() {PIP, Tinv}

        End Function

        Public Shared Function CalcPIPressure(Vx As Double(), Pest As Double, T As Double, pp As PropertyPackage, ByVal eos As String) As String

            Dim P, PIP As Double

            Dim brent As New MathEx.BrentOpt.Brent
            brent.DefineFuncDelegate(AddressOf PIPressureF)

            P = brent.BrentOpt(101325, Pest, 100, 0.001, 1000, New Object() {Vx, T, pp, eos})

            PIP = CalcPIP(Vx, P, T, pp, eos)(0)

            If P < 0 Or Abs(P - Pest) <= (Pest - 101325) / 1000 Then P = 0.0#

            Return P

        End Function

        Private Shared Function PIPressureF(x As Double, otherargs As Object)

            Return 1 - CalcPIP(otherargs(0), x, otherargs(1), otherargs(2), otherargs(3))(0)

        End Function

#End Region

    End Class

    ''' <summary>
    ''' Class to store flash calculation results.
    ''' </summary>
    ''' <remarks></remarks>
    <System.Serializable> Public Class FlashCalculationResult

        Implements Interfaces.IFlashCalculationResult

        ''' <summary>
        ''' Defines the base mole amount for determination of phase/compound fractions. Default is 1.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property BaseMoleAmount As Double = 1.0# Implements Interfaces.IFlashCalculationResult.BaseMoleAmount
        Public Property Kvalues As New List(Of Double) Implements Interfaces.IFlashCalculationResult.Kvalues
        Public Property MixtureMoleAmounts As New List(Of Double) Implements Interfaces.IFlashCalculationResult.MixtureMoleAmounts
        Public Property VaporPhaseMoleAmounts As New List(Of Double) Implements Interfaces.IFlashCalculationResult.VaporPhaseMoleAmounts
        Public Property LiquidPhase1MoleAmounts As New List(Of Double) Implements Interfaces.IFlashCalculationResult.LiquidPhase1MoleAmounts
        Public Property LiquidPhase2MoleAmounts As New List(Of Double) Implements Interfaces.IFlashCalculationResult.LiquidPhase2MoleAmounts
        Public Property SolidPhaseMoleAmounts As New List(Of Double) Implements Interfaces.IFlashCalculationResult.SolidPhaseMoleAmounts
        Public Property CalculatedTemperature As Nullable(Of Double) Implements Interfaces.IFlashCalculationResult.CalculatedTemperature
        Public Property CalculatedPressure As Nullable(Of Double) Implements Interfaces.IFlashCalculationResult.CalculatedPressure
        Public Property CalculatedEnthalpy As Nullable(Of Double) Implements Interfaces.IFlashCalculationResult.CalculatedEnthalpy
        Public Property CalculatedEntropy As Nullable(Of Double) Implements Interfaces.IFlashCalculationResult.CalculatedEntropy
        Public Property CompoundProperties As List(Of Interfaces.ICompoundConstantProperties) Implements Interfaces.IFlashCalculationResult.CompoundProperties
        Public Property FlashAlgorithmType As String = "" Implements Interfaces.IFlashCalculationResult.FlashAlgorithmType
        Public Property FlashSpecification1 As PropertyPackages.FlashSpec
        Public Property FlashSpecification2 As PropertyPackages.FlashSpec
        Public Property ResultException As Exception Implements Interfaces.IFlashCalculationResult.ResultException
        Public Property IterationsTaken As Integer = 0 Implements Interfaces.IFlashCalculationResult.IterationsTaken
        Public Property TimeTaken As New TimeSpan() Implements Interfaces.IFlashCalculationResult.TimeTaken

        Sub New()

        End Sub

        Sub New(constprop As List(Of Interfaces.ICompoundConstantProperties))

            CompoundProperties = constprop

        End Sub

        Public Function GetVaporPhaseMoleFractions() As Double() Implements Interfaces.IFlashCalculationResult.GetVaporPhaseMoleFractions

            Dim collection As List(Of Double) = VaporPhaseMoleAmounts

            Dim total As Double = collection.ToArray.SumY

            If total = 0.0# Then total = 1.0#

            Dim molefracs As New List(Of Double)

            For Each value As Double In collection
                molefracs.Add(value / total)
            Next

            Return molefracs.ToArray

        End Function

        Public Function GetLiquidPhase1MoleFractions() As Double() Implements Interfaces.IFlashCalculationResult.GetLiquidPhase1MoleFractions

            Dim collection As List(Of Double) = LiquidPhase1MoleAmounts

            Dim total As Double = collection.ToArray.SumY

            If total = 0.0# Then total = 1.0#

            Dim molefracs As New List(Of Double)

            For Each value As Double In collection
                molefracs.Add(value / total)
            Next

            Return molefracs.ToArray

        End Function

        Public Function GetLiquidPhase2MoleFractions() As Double() Implements Interfaces.IFlashCalculationResult.GetLiquidPhase2MoleFractions

            Dim collection As List(Of Double) = LiquidPhase2MoleAmounts

            Dim total As Double = collection.ToArray.SumY

            If total = 0.0# Then total = 1.0#

            Dim molefracs As New List(Of Double)

            For Each value As Double In collection
                molefracs.Add(value / total)
            Next

            Return molefracs.ToArray

        End Function

        Public Function GetSolidPhaseMoleFractions() As Double() Implements Interfaces.IFlashCalculationResult.GetSolidPhaseMoleFractions

            Dim collection As List(Of Double) = SolidPhaseMoleAmounts

            Dim total As Double = collection.ToArray.SumY

            If total = 0.0# Then total = 1.0#

            Dim molefracs As New List(Of Double)

            For Each value As Double In collection
                molefracs.Add(value / total)
            Next

            Return molefracs.ToArray

        End Function

        Public Function GetVaporPhaseMoleFraction() As Double Implements Interfaces.IFlashCalculationResult.GetVaporPhaseMoleFraction

            Dim collection As List(Of Double) = VaporPhaseMoleAmounts

            Return collection.ToArray.SumY

        End Function

        Public Function GetLiquidPhase1MoleFraction() As Double Implements Interfaces.IFlashCalculationResult.GetLiquidPhase1MoleFraction

            Dim collection As List(Of Double) = LiquidPhase1MoleAmounts

            Return collection.ToArray.SumY

        End Function

        Public Function GetLiquidPhase2MoleFraction() As Double Implements Interfaces.IFlashCalculationResult.GetLiquidPhase2MoleFraction

            Dim collection As List(Of Double) = LiquidPhase2MoleAmounts

            Return collection.ToArray.SumY

        End Function

        Public Function GetSolidPhaseMoleFraction() As Double Implements Interfaces.IFlashCalculationResult.GetSolidPhaseMoleFraction

            Dim collection As List(Of Double) = SolidPhaseMoleAmounts

            Return collection.ToArray.SumY

        End Function

        Public Function GetVaporPhaseMassFractions() As Double() Implements Interfaces.IFlashCalculationResult.GetVaporPhaseMassFractions

            Return ConvertToMassFractions(GetVaporPhaseMoleFractions())

        End Function

        Public Function GetLiquidPhase1MassFractions() As Double() Implements Interfaces.IFlashCalculationResult.GetLiquidPhase1MassFractions

            Return ConvertToMassFractions(GetLiquidPhase1MoleFractions())

        End Function

        Public Function GetLiquidPhase2MassFractions() As Double() Implements Interfaces.IFlashCalculationResult.GetLiquidPhase2MassFractions

            Return ConvertToMassFractions(GetLiquidPhase2MoleFractions())

        End Function

        Public Function GetSolidPhaseMassFractions() As Double() Implements Interfaces.IFlashCalculationResult.GetSolidPhaseMassFractions

            Return ConvertToMassFractions(GetSolidPhaseMoleFractions())

        End Function

        Private Function ConvertToMassFractions(ByVal Vz As Double()) As Double() Implements Interfaces.IFlashCalculationResult.ConvertToMassFractions

            Dim Vwe(UBound(Vz)) As Double
            Dim mol_x_mm As Double = 0
            Dim i As Integer = 0
            For Each sub1 As Interfaces.ICompoundConstantProperties In CompoundProperties
                mol_x_mm += Vz(i) * sub1.Molar_Weight
                i += 1
            Next

            If mol_x_mm = 0.0# Then mol_x_mm = 1.0#

            i = 0
            For Each sub1 As Interfaces.ICompoundConstantProperties In CompoundProperties
                If mol_x_mm <> 0 Then
                    Vwe(i) = Vz(i) * sub1.Molar_Weight / mol_x_mm
                Else
                    Vwe(i) = 0.0#
                End If
                i += 1
            Next

            Return Vwe

        End Function

        Private Function CalcMolarWeight(ByVal Vz() As Double) As Double Implements Interfaces.IFlashCalculationResult.CalcMolarWeight

            Dim val As Double

            Dim i As Integer = 0

            For Each subst As Interfaces.ICompoundConstantProperties In CompoundProperties
                val += Vz(i) * subst.Molar_Weight
                i += 1
            Next

            Return val

        End Function

        Public Function GetVaporPhaseMassFraction() As Double Implements Interfaces.IFlashCalculationResult.GetVaporPhaseMassFraction

            Dim mw, vw As Double

            mw = MixtureMoleAmounts.Sum * CalcMolarWeight(MixtureMoleAmounts.ToArray.MultiplyConstY(1 / BaseMoleAmount))
            vw = VaporPhaseMoleAmounts.Sum * CalcMolarWeight(GetVaporPhaseMoleFractions())

            Return vw / mw

        End Function

        Public Function GetLiquidPhase1MassFraction() As Double Implements Interfaces.IFlashCalculationResult.GetLiquidPhase1MassFraction

            Dim mw, l1w As Double

            mw = MixtureMoleAmounts.Sum * CalcMolarWeight(MixtureMoleAmounts.ToArray.MultiplyConstY(1 / BaseMoleAmount))
            l1w = LiquidPhase1MoleAmounts.Sum * CalcMolarWeight(GetLiquidPhase1MoleFractions())

            Return l1w / mw

        End Function

        Public Function GetLiquidPhase2MassFraction() As Double Implements Interfaces.IFlashCalculationResult.GetLiquidPhase2MassFraction

            Dim mw, l2w As Double

            mw = MixtureMoleAmounts.Sum * CalcMolarWeight(MixtureMoleAmounts.ToArray.MultiplyConstY(1 / BaseMoleAmount))
            l2w = LiquidPhase2MoleAmounts.Sum * CalcMolarWeight(GetLiquidPhase2MoleFractions())

            Return l2w / mw

        End Function

        Public Function GetSolidPhaseMassFraction() As Double Implements Interfaces.IFlashCalculationResult.GetSolidPhaseMassFraction

            Dim mw, sw As Double

            mw = MixtureMoleAmounts.Sum * CalcMolarWeight(MixtureMoleAmounts.ToArray.MultiplyConstY(1 / BaseMoleAmount))
            sw = SolidPhaseMoleAmounts.Sum * CalcMolarWeight(GetSolidPhaseMoleFractions())

            Return sw / mw

        End Function

    End Class

End Namespace
