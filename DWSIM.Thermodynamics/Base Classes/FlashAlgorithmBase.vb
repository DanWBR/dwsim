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
Imports System.IO
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Runtime.Serialization

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    ''' <summary>
    ''' This is the base class for the flash algorithms.
    ''' </summary>
    ''' <remarks></remarks>
    <System.Serializable()> Public MustInherit Class FlashAlgorithm

        Implements Interfaces.IFlashAlgorithm, Interfaces.ICustomXMLSerialization

        Public Property Order As Integer = 1000 Implements Interfaces.IFlashAlgorithm.Order

        Public Property FlashSettings As New Dictionary(Of Interfaces.Enums.FlashSetting, String) Implements Interfaces.IFlashAlgorithm.FlashSettings

        Public Property StabSearchSeverity As Integer
            Get
                Return Integer.Parse(FlashSettings(ThreePhaseFlashStabTestSeverity))
            End Get
            Set(value As Integer)
                FlashSettings(ThreePhaseFlashStabTestSeverity) = value.ToString
            End Set
        End Property

        Public Property StabSearchCompIDs As String()
            Get
                Dim list1 As String() = FlashSettings(ThreePhaseFlashStabTestCompIds).ToArray(System.Globalization.CultureInfo.CurrentCulture, Type.GetType("System.String"))
                Dim list2 = list1.ToList()
                list2.Remove("")
                Return list2.ToArray
            End Get
            Set(value As String())
                Dim comps As String = ""
                For Each s As String In value
                    comps += s + ","
                Next
                FlashSettings(ThreePhaseFlashStabTestCompIds) = comps
            End Set
        End Property

        Private _P As Double, _Vz, _Vx1est, _Vx2est As Double(), _pp As PropertyPackage

        Sub New()

            FlashSettings = GetDefaultSettings()

        End Sub

        Public Overrides Function ToString() As String
            If Name <> "" Then
                Return Name
            Else
                Return MyBase.ToString()
            End If
        End Function

        Public Shared Function GetDefaultSettings() As Dictionary(Of Interfaces.Enums.FlashSetting, String)

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            Dim settings As New Dictionary(Of Interfaces.Enums.FlashSetting, String)

            settings(Interfaces.Enums.FlashSetting.Replace_PTFlash) = False
            settings(Interfaces.Enums.FlashSetting.ValidateEquilibriumCalc) = False
            settings(Interfaces.Enums.FlashSetting.UsePhaseIdentificationAlgorithm) = False
            settings(Interfaces.Enums.FlashSetting.CalculateBubbleAndDewPoints) = False

            settings(Interfaces.Enums.FlashSetting.ValidationGibbsTolerance) = (0.01).ToString(ci)

            settings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations) = Integer.Parse(100).ToString(ci)
            settings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance) = Double.Parse(0.0001).ToString(ci)
            settings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations) = Double.Parse(100).ToString(ci)
            settings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance) = Double.Parse(0.0001).ToString(ci)
            settings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations) = Double.Parse(100).ToString(ci)
            settings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance) = Double.Parse(0.0001).ToString(ci)
            settings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations) = Double.Parse(100).ToString(ci)
            settings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance) = Double.Parse(0.0001).ToString(ci)

            settings(Interfaces.Enums.FlashSetting.NL_FastMode) = True

            settings(Interfaces.Enums.FlashSetting.IO_FastMode) = True

            settings(Interfaces.Enums.FlashSetting.GM_OptimizationMethod) = "IPOPT"

            settings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 0

            settings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestCompIds) = ""

            settings(Interfaces.Enums.FlashSetting.PVFlash_FixedDampingFactor) = 1.0.ToString(ci)
            settings(Interfaces.Enums.FlashSetting.PVFlash_MaximumTemperatureChange) = 10.0.ToString(ci)
            settings(Interfaces.Enums.FlashSetting.PVFlash_TemperatureDerivativeEpsilon) = 0.1.ToString(ci)

            settings(Interfaces.Enums.FlashSetting.ST_Number_of_Random_Tries) = 20

            settings(Interfaces.Enums.FlashSetting.CheckIncipientLiquidForStability) = False

            Return settings

        End Function

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
                        .CalculatedPressure = val1
                        .CalculatedTemperature = val2
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
                        .CalculatedPressure = val2
                        .CalculatedTemperature = val1
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
                        .CalculatedPressure = val1
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
                        .CalculatedPressure = val1
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
                        .CalculatedPressure = val1
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
                        .CalculatedTemperature = val1
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
                        .CalculatedPressure = val1
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

                Throw ex

            End Try

            Return calcresult

        End Function

        Function CalculateMixtureEnthalpy(ByVal T As Double, ByVal P As Double, ByVal L As Double, ByVal L2 As Double, ByVal V As Double, ByVal S As Double,
                                          ByVal Vx As Double(), ByVal Vx2 As Double(), ByVal Vy As Double(), ByVal Vs As Double(), ByVal pp As PropertyPackage) As Double

            Dim _Hv, _Hl, _Hl2, _Hs As Double

            Dim n As Integer = Vx.Length - 1

            _Hv = 0
            _Hl = 0
            _Hl2 = 0
            _Hs = 0

            Dim mmg, mml, mml2, mms As Double

            If V > 0.0# And Vy.Sum > 0.0# Then _Hv = pp.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
            If L > 0.0# And Vx.Sum > 0.0# Then _Hl = pp.DW_CalcEnthalpy(Vx, T, P, State.Liquid)
            If L2 > 0.0# And Vx2.Sum > 0.0# Then _Hl2 = pp.DW_CalcEnthalpy(Vx2, T, P, State.Liquid)
            If S > 0.0# And Vs.Sum > 0.0# Then _Hs = pp.DW_CalcEnthalpy(Vs, T, P, State.Solid)

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

            Dim n As Integer = Vx.Length - 1

            _Sv = 0
            _Sl = 0
            _Sl2 = 0

            Dim mmg, mml, mml2, mms As Double

            If V > 0.0# And Vy.Sum > 0.0# Then _Sv = pp.DW_CalcEntropy(Vy, T, P, State.Vapor)
            If L > 0.0# And Vx.Sum > 0.0# Then _Sl = pp.DW_CalcEntropy(Vx, T, P, State.Liquid)
            If L2 > 0.0# And Vx2.Sum > 0.0# Then _Sl2 = pp.DW_CalcEntropy(Vs, T, P, State.Solid)

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

        Public Function StabTest(ByVal T As Double, ByVal P As Double, ByVal Vz As Double(), ByVal VTc As Double(), ByVal pp As PropertyPackage)

            If pp.AUX_IS_SINGLECOMP(Vz) Then
                Return New Object() {True, Nothing}
            End If

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "StabTest", Name & " (Stability Test)", "Liquid Phase Stability Test Routine", True)

            IObj?.Paragraphs.Add("Stability analysis represents the most challenging problem associated with multiphase flash calculations. 
The phase split calculation, which given the number of phases leads to locating an unconstrained local minimum, is essentially a purely 
technical problem, where the choice of solution procedure affects speed rather than reliability. The stability analysis, in contrast, 
requires the determination of a global minimum, with no advance information on the location of this minimum. Any practical implementation 
of multiphase stability analysis has to balance speed of execution against reliability, the more extensive, and thus more costly, search 
being less likely to overlook indications of instability. Previous algorithms have mostly been based on partly empirical observations 
relating to the characteristics of multiphase equilibrium, selecting trial phase compositions in the manner most likely to yield conclusive 
information. Currently, 'safe' algorithms that are guaranteed to resolve the stability question are under active investigation by many groups. 
The techniques used comprise global optimization methods and interval analysis, and currently the time expenditure for the calculations appear 
prohibitive for more complex problems.")

            IObj?.Paragraphs.Add("The essential difference between stability analysis for two-phase (vapour/liquid) and multiphase problems 
is that selection of two trial phase compositions (with subsequent local minimization) is adequate and feasible — in the first case, whereas
even selection of as many trial phases as the number of components in the mixture may not be sufficient in the latter. For mixtures containing 
10 or more components, converging the tangent plane distance minimization for a large number of individual initial estimates represents a 
substantial effort, and we therefore suggest the use of a screening procedure rather than a full search. The outcome of the screening procedure 
then decides which of the trial phases to investigate further.")

            IObj?.Paragraphs.Add("<h3>Selection of initial estimates for stability analysis</h3>")

            IObj?.Paragraphs.Add("Assume that the current status of a multiphase equilibrium calculation is that a local minimum in the Gibbs 
energy corresponding to F phases has been determined, i.e.")

            IObj?.Paragraphs.Add("<m>\ln f_{iL}=\ln f_{i2} = ... = \ln f_{iF}\space\space(=\ln f^*_i)</m>")

            IObj?.Paragraphs.Add("where each of the F phases")

            IObj?.Paragraphs.Add("<m>\mathbf{n}^1 ,\mathbf{n}^2,...,\mathbf{n}^F</m>")

            IObj?.Paragraphs.Add("are intrinsically stable. ")

            IObj?.Paragraphs.Add("The tangent plane distance for a trial phase of composition w is given by")

            IObj?.Paragraphs.Add("<m>tpd(\mathbf{w})=\sum\limits_{i}{w_i}(\ln f_i(\mathbf{w})-\ln f^*_i)=\sum\limits_{i}{w_i(\ln w_i +\ln \varphi _i(\mathbf{w})-d_i)} </m>")

            IObj?.Paragraphs.Add("with <mi>d_i = \ln (f^*_i/P)</mi> and if <mi>tpd(\mathbf{w})</mi> is non-negative for all w the equilibrium 
phase distribution is stable and no further calculation is required. If, on the other hand, a composition w with a negative tangent plane 
distance can be located, the phase distribution is unstable and the composition w can be utilised for generating initial estimates for an 
F+l-phase calculation, as it is known that the mixture Gibbs energy can be reduced by introducing a small amount of a phase with this composition.")

            IObj?.Paragraphs.Add("In practice we shall test a sequence of composition estimates w for negative tangent plane distances. 
If none of the trial phases verify instability, the mixture is assumed to be stable. One of the objectives is to select and evaluate 
the set of trial phase compositions most likely to indicate instability and thus to form new phases. Another objective is to perform 
the  for a given test phase as economically as possible.")

            IObj?.Paragraphs.Add("When a trial phase has been chosen, refinement is performed by means of successive substitution, 
as for the two-phase flash. Continuation of successivo substitution until convergence will locate either a non-trivial minimum, i.e. 
a minimum with a composition different from that of any of the equilibrium phases, or a trivial solution. The aim of the screening 
procedure is to decide at an early stage whether convergence to a trivial solution is likely to occur, in which case further iteration 
on the chosen trial phase can be abandoned, Obviously, increasing the number of successive substitution steps increases the reliability 
of the screening procedure, as well as the associated cost.")

            IObj?.Paragraphs.Add("In the procedure of Michelsen, 2-4 steps of successive substitution  performed in parallel for each 
trial phase. If instability (negative till) is not encountered during these steps, only the trial phase composition with the smaller, 
decreasing U-value is converged. The screening procedure is evidently empirical and cannot be guaranteed to succeed, but practical 
experience indicates that with proper selection of the initial phase compositionsthe approach represents a reasonable compromise 
between reliability and cost.")

            IObj?.Paragraphs.Add("<h3>Selection of trial phase compositions</h3>")

            IObj?.Paragraphs.Add("In the absence of any advance knowledge about the nature of the mixture to be flashed, at least C + 1 
different initial trial phases are required. One of these is used to search for a vapour (fortunately, equilibrium comprises at most 
one vapour phase), and the remaining C trial phases cater for the possibility of formation of a liquid phase rich in the corresponding 
mixture component.")

            IObj?.Paragraphs.Add("The initial composition of the vapour phase is calculated as Wi = exp(di), based on the assumption 
that the trial phase is an ideal vapour (with a fugacity coefficient of 1), and in all subsequent iterations properties of this trial 
phases are calculated using the vapour density, if the equation of state has multiple roots.")

            IObj?.Paragraphs.Add("The additional C trial phases are initiated as the respective pure components, properties of the trial 
phase being calculated with the liquid density, as the purpose of this search is to reveal the potential formation of new liquid like 
phases. Starting from each 'corner' of the composition space aims at ensuring that the search will cover the entire region as well as possible. 
The use of pure trial phases also ensures rapid detection of instability with highly immiscible components.")

            IObj?.Paragraphs.Add("Unfortunately, even converging all C + 1 trial phase compositions does not guarantee that the global 
minimum of the tangent plane distance will be located, and many practically important exceptions are found. These are characterised by 
the existence of a liquid phase dominated by a light component, e.g. methane or carbon dioxide, under conditions where this component is 
unable to exist as a pure liquid. As a consequence the pure component initialization is incapable of creating the liquid phase rich in
this particular component, and there is no certainty that the alternative initializations, which start far from the desired minimum, 
will converge to this solution.")

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", pp.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))

            WriteDebugInfo("Starting Liquid Phase Stability Test @ T = " & T & " K & P = " & P & " Pa for the following trial phases:")

            IObj?.Paragraphs.Add("Starting Liquid Phase Stability Test @ T = " & T & " K & P = " & P & " Pa for the following trial phases:")

            Dim i, j, n, o, l, nt, maxits As Integer
            n = Vz.Length - 1
            nt = n

            Dim Vtrials As New List(Of Double())
            Dim Vestimates As New Concurrent.ConcurrentBag(Of Double())
            Dim idx(nt) As Integer

            'For j = 0 To n
            '    If Vz(j) > 0 And T < VTc(i) Then Vtrials.Add(pp.RET_NullVector)
            'Next

            For j = 0 To n
                Vtrials.Add(pp.RET_NullVector)
            Next

            For Each vector In Vtrials
                For i = 0 To n
                    vector(i) = 0.000001
                Next
            Next

            For j = 0 To Vtrials.Count - 1
                Vtrials(j)(j) = 1.0
            Next

            'For j = 0 To n
            '    If T < VTc(j) Then Vtrials.RemoveAt(j)
            'Next

            Dim tol As Double
            Dim fcv(n), fcl(n) As Double

            tol = 0.00001
            maxits = 200

            Dim h(n), lnfi_z(n) As Double

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
                IObj?.SetCurrent
                fcv = pp.DW_CalcFugCoeff(Vz, T, P, State.Vapor)
                IObj?.SetCurrent
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

            Vtrials.Add(pp.RET_NullVector)
            Vtrials.Add(pp.RET_NullVector)

            i = 0
            Do
                If Vz(i) <> 0.0# Then
                    Vtrials(n + 1)(i) = Exp(h(i))
                Else
                    Vtrials(n + 1)(i) = 0.0#
                End If
                Vtrials(n + 2)(i) = 1.0 / (n + 1)
                i = i + 1
            Loop Until i = n + 1

            Dim ntries As Integer = FlashSettings(Interfaces.Enums.FlashSetting.ST_Number_of_Random_Tries)

            For i = 0 To ntries
                Dim random As New Random(i)
                Vtrials.Add(Enumerable.Repeat(0, n + 1).Select(Function(d) random.NextDouble()).ToArray.MultiplyY(Vz))
            Next

            For i = 0 To Vtrials.Count - 1
                Vtrials(i) = Vtrials(i).NormalizeY()
            Next

            Dim m As Integer = Vtrials.Count - 1 '+ 2

            Dim g_(m), beta(m), r(m), r_ant(m) As Double
            Dim excidx As New Concurrent.ConcurrentBag(Of Integer)

            Dim prevstatus = GlobalSettings.Settings.InspectorEnabled

            GlobalSettings.Settings.InspectorEnabled = False

            'start stability test for each one of the initial estimate vectors
            Parallel.For(0, m + 1, Sub(xi)

                                       Dim jj, cc As Integer
                                       Dim vector = Vtrials(xi)

                                       Dim lnfi(n), Y(n), Y_ant(n) As Double
                                       Dim currcomp(n) As Double
                                       Dim dgdY(n), tmpfug(n), dY(n), sum3, ffcv(), ffcl(), ttmpfug() As Double
                                       Dim finish As Boolean = True

                                       Y = vector.Clone

                                       cc = 0
                                       Do

                                           If Not excidx.Contains(xi) Then
                                               jj = 0
                                               sum3 = 0
                                               Do
                                                   If Y(jj) > 0 Then sum3 += Y(jj)
                                                   jj = jj + 1
                                               Loop Until jj = n + 1
                                               jj = 0
                                               Do
                                                   If Y(jj) > 0 Then currcomp(jj) = Y(jj) / sum3 Else currcomp(jj) = 0
                                                   jj = jj + 1
                                               Loop Until jj = n + 1

                                               ffcv = pp.DW_CalcFugCoeff(currcomp, T, P, State.Vapor)
                                               ffcl = pp.DW_CalcFugCoeff(currcomp, T, P, State.Liquid)

                                               Dim ggv As Double = 0.0#
                                               Dim ggl As Double = 0.0#
                                               For jj = 0 To n
                                                   If currcomp(jj) <> 0.0# Then ggv += currcomp(jj) * Log(ffcv(jj) * currcomp(jj))
                                                   If currcomp(jj) <> 0.0# Then ggl += currcomp(jj) * Log(ffcl(jj) * currcomp(jj))
                                               Next

                                               If ggl <= ggv Then
                                                   ttmpfug = ffcl
                                               Else
                                                   ttmpfug = ffcv
                                               End If

                                               jj = 0
                                               Do
                                                   lnfi(jj) = Log(ttmpfug(jj))
                                                   jj = jj + 1
                                               Loop Until jj = n + 1
                                               jj = 0
                                               Do
                                                   dgdY(jj) = Log(Y(jj)) + lnfi(jj) - h(jj)
                                                   jj = jj + 1
                                               Loop Until jj = n + 1
                                               jj = 0
                                               beta(xi) = 0
                                               Do
                                                   beta(xi) += (Y(jj) - Vz(jj)) * dgdY(jj)
                                                   jj = jj + 1
                                               Loop Until jj = n + 1
                                               g_(xi) = 1
                                               jj = 0
                                               Do
                                                   g_(xi) += Y(jj) * (Log(Y(jj)) + lnfi(jj) - h(jj) - 1)
                                                   jj = jj + 1
                                               Loop Until jj = n + 1
                                               If xi > 0 Then r_ant(xi) = r(xi) Else r_ant(xi) = 0
                                               r(xi) = 2 * g_(xi) / beta(xi)
                                           End If

                                           jj = 0
                                           Do
                                               Y_ant(jj) = Y(jj)
                                               Y(jj) = Exp(h(jj) - lnfi(jj))
                                               dY(jj) = Y(jj) - Y_ant(jj)
                                               If Y(jj) < 0 Then Y(jj) = 0
                                               jj = jj + 1
                                           Loop Until jj = n + 1

                                           'check convergence

                                           finish = True
                                           jj = 0
                                           Do
                                               If Abs(dY(jj)) > tol Then
                                                   finish = False
                                               End If
                                               jj = jj + 1
                                           Loop Until jj = n + 1

                                           cc = cc + 1

                                           If cc > maxits Then Exit Do

                                           If finish Then Vestimates.Add(Y)

                                           If Double.IsNaN(Y.SumY) Then Exit Do

                                       Loop Until finish = True

                                   End Sub)

            GlobalSettings.Settings.InspectorEnabled = prevstatus

            IObj?.SetCurrent

            ' search for trivial solutions

            i = 0
            Do
                If (Abs(g_(i)) < 0.0000000001 And r(i) > 0.9 And r(i) < 1.1) Then
                    If Not excidx.Contains(i) Then excidx.Add(i)
                End If
                i = i + 1
            Loop Until i = m + 1


            Dim sum As Double
            i = 0
            For Each vector In Vestimates
                If Not excidx.Contains(i) Then
                    j = 0
                    sum = 0
                    Do
                        sum += Abs(vector(j) - Vz(j))
                        j = j + 1
                    Loop Until j = n + 1
                    If sum < 0.001 Then
                        If Not excidx.Contains(i) Then excidx.Add(i)
                    End If
                End If
                i += 1
            Next

            ' search for trivial solutions

            Dim sum5 As Double
            i = 0
            For Each vector In Vestimates
                If Not excidx.Contains(i) Then
                    j = 0
                    sum5 = 0
                    Do
                        sum5 += vector(j)
                        j = j + 1
                    Loop Until j = n + 1
                    If Abs(sum5 - 1) < 0.001 Or Double.IsNaN(sum5) Or Double.IsInfinity(sum5) Then
                        'phase is stable
                        If Not excidx.Contains(i) Then excidx.Add(i)
                    End If
                End If
                i = i + 1
            Next

            ' join similar solutions

            Dim similar As Boolean

            i = 0
            For Each vector In Vestimates
                If Not excidx.Contains(i) Then
                    o = 0
                    Do
                        If Not excidx.Contains(o) And i <> o Then
                            similar = True
                            j = 0
                            Do
                                If Abs(vector(j) - Vestimates(o)(j)) > 0.00001 Then
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
                    Loop Until o = Vestimates.Count - 1
                End If
                i = i + 1
            Next

            l = excidx.Count
            Dim sum2 As Double
            Dim isStable As Boolean

            m = Vestimates.Count - 1

            If m + 1 - l > 0 Then

                'the phase is unstable

                isStable = False

                'normalize initial estimates

                IObj?.Paragraphs.Add("Liquid Phase Stability Test finished. Phase is NOT stable. Initial estimates for incipient liquid phase composition:")
                IObj?.Close()

                WriteDebugInfo("Liquid Phase Stability Test finished. Phase is NOT stable. Initial estimates for incipient liquid phase composition:")

                Dim inest(m - l, n) As Double
                i = 0
                l = 0
                Do
                    If Not excidx.Contains(i) Then
                        Dim text As String = "{"
                        j = 0
                        sum2 = 0
                        Do
                            sum2 += Vestimates(i)(j)
                            j = j + 1
                        Loop Until j = n + 1
                        j = 0
                        Do
                            inest(l, j) = Vestimates(i)(j) / sum2
                            text += inest(l, j).ToString & vbTab
                            j = j + 1
                        Loop Until j = n + 1
                        text.TrimEnd(New Char() {vbTab})
                        text += "}"
                        IObj?.Paragraphs.Add(text)
                        WriteDebugInfo(text)
                        l = l + 1
                    End If
                    i = i + 1
                Loop Until i = m + 1
                Return New Object() {isStable, inest}
            Else

                'the phase is stable

                WriteDebugInfo("Liquid Phase Stability Test finished. Phase is stable.")

                IObj?.Paragraphs.Add("Liquid Phase Stability Test finished. Phase is stable.")
                IObj?.Close()

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

            P = brent.BrentOpt(1, Pest, 100, 0.001, 1000, New Object() {Vx, T, pp, eos})

            PIP = CalcPIP(Vx, P, T, pp, eos)(0)

            If P < 0 Or Abs(P - Pest) <= (Pest - 101325) / 1000 Then P = 0.0#

            Return P

        End Function

        Private Shared Function PIPressureF(x As Double, otherargs As Object)

            Return 1 - CalcPIP(otherargs(0), x, otherargs(1), otherargs(2), otherargs(3))(0)

        End Function

#End Region

#Region "XML Serialization"

        Public Overridable Function LoadData(data As List(Of XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            Dim el = (From xel As XElement In data Select xel Where xel.Name = "FlashSettings").SingleOrDefault

            If Not el Is Nothing Then

                FlashSettings.Clear()

                For Each xel3 In el.Elements
                    Try
                        Dim esname = [Enum].Parse(Interfaces.Enums.Helpers.GetEnumType("DWSIM.Interfaces.Enums.FlashSetting"), xel3.@Name)
                        FlashSettings.Add(esname, xel3.@Value)
                    Catch ex As Exception
                    End Try
                Next

                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.PVFlash_FixedDampingFactor) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.PVFlash_FixedDampingFactor, 1.0.ToString(Globalization.CultureInfo.InvariantCulture))
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.PVFlash_MaximumTemperatureChange) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.PVFlash_MaximumTemperatureChange, 10.0.ToString(Globalization.CultureInfo.InvariantCulture))
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.PVFlash_TemperatureDerivativeEpsilon) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.PVFlash_TemperatureDerivativeEpsilon, 0.1.ToString(Globalization.CultureInfo.InvariantCulture))
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.ST_Number_of_Random_Tries) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.ST_Number_of_Random_Tries, 20)
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.CheckIncipientLiquidForStability) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.CheckIncipientLiquidForStability, False)
                End If

            End If

            Return XMLSerializer.XMLSerializer.Deserialize(Me, data)

        End Function

        Public Overridable Function SaveData() As List(Of XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me)

            elements.Add(New XElement("FlashSettings"))

            For Each item In FlashSettings
                elements(elements.Count - 1).Add(New XElement("Setting", New XAttribute("Name", item.Key), New XAttribute("Value", item.Value)))
            Next

            Return elements

        End Function

#End Region

        Public Function Clone() As Interfaces.IFlashAlgorithm Implements Interfaces.IFlashAlgorithm.Clone

            Dim clonedobj As FlashAlgorithm = Me.MemberwiseClone()
            clonedobj.FlashSettings = New Dictionary(Of Interfaces.Enums.FlashSetting, String)
            For Each item In Me.FlashSettings
                clonedobj.FlashSettings.Add(item.Key, item.Value)
            Next
            Return clonedobj

        End Function

        Public MustOverride ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod Implements Interfaces.IFlashAlgorithm.AlgoType

        Public MustOverride ReadOnly Property Description As String Implements Interfaces.IFlashAlgorithm.Description

        Public MustOverride ReadOnly Property Name As String Implements Interfaces.IFlashAlgorithm.Name

        Public Overridable ReadOnly Property InternalUseOnly As Boolean Implements Interfaces.IFlashAlgorithm.InternalUseOnly
            Get
                Return False
            End Get
        End Property

        Public Property Tag As String = "" Implements Interfaces.IFlashAlgorithm.Tag

        Public MustOverride ReadOnly Property MobileCompatible As Boolean Implements Interfaces.IFlashAlgorithm.MobileCompatible

    End Class

    ''' <summary>
    ''' Class to store flash calculation results.
    ''' </summary>
    ''' <remarks></remarks>
    ''' 
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

            Dim Vwe(Vz.Length - 1) As Double
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
