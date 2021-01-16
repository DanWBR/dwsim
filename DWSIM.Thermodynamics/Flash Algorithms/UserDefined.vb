'    User-Defined (Python) Flash Algorithm Routines
'    Copyright 2019 Daniel Wagner O. de Medeiros
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

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    Public Class UserDefined

        Inherits FlashAlgorithm

        Public PTFlash As Func(Of Double(), Double, Double, PropertyPackage, FlashCalculationResult)
        Public PHFlash As Func(Of Double(), Double, Double, Double, PropertyPackage, FlashCalculationResult)
        Public PSFlash As Func(Of Double(), Double, Double, Double, PropertyPackage, FlashCalculationResult)
        Public PVFlash As Func(Of Double(), Double, Double, Double, PropertyPackage, FlashCalculationResult)
        Public TVFlash As Func(Of Double(), Double, Double, Double, PropertyPackage, FlashCalculationResult)

        Public Sub New()
            MyBase.New
            Order = 100
        End Sub

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod
            Get
                Return Interfaces.Enums.FlashMethod.UserDefined
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String
            Get
                Return "User-Defined (Python) Flash Algorithm Routines"
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                Return "User-Defined"
            End Get
        End Property

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

        Public Overrides Function Flash_PT(Vz() As Double, P As Double, T As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object

            Dim result As FlashCalculationResult = PTFlash.Invoke(Vz, P, T, PP)

            Return New Object() {result.GetLiquidPhase1MoleFraction,
                                result.GetVaporPhaseMoleFraction,
                                result.GetLiquidPhase1MoleFractions,
                                result.GetVaporPhaseMoleFractions,
                                result.IterationsTaken,
                                result.GetLiquidPhase2MoleFraction,
                                result.GetLiquidPhase2MoleFractions,
                                result.GetSolidPhaseMoleFraction,
                                result.GetSolidPhaseMoleFractions}

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim result As FlashCalculationResult = PHFlash.Invoke(Vz, P, H, Tref, PP)

            Return New Object() {result.GetLiquidPhase1MoleFraction,
                                result.GetVaporPhaseMoleFraction,
                                result.GetLiquidPhase1MoleFractions,
                                result.GetVaporPhaseMoleFractions,
                                result.CalculatedTemperature,
                                result.IterationsTaken,
                                result.Kvalues,
                                result.GetLiquidPhase2MoleFraction,
                                result.GetLiquidPhase2MoleFractions,
                                result.GetSolidPhaseMoleFraction,
                                result.GetSolidPhaseMoleFractions}

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim result As FlashCalculationResult = PSFlash.Invoke(Vz, P, S, Tref, PP)

            Return New Object() {result.GetLiquidPhase1MoleFraction,
                                result.GetVaporPhaseMoleFraction,
                                result.GetLiquidPhase1MoleFractions,
                                result.GetVaporPhaseMoleFractions,
                                result.CalculatedTemperature,
                                result.IterationsTaken,
                                result.Kvalues,
                                result.GetLiquidPhase2MoleFraction,
                                result.GetLiquidPhase2MoleFractions,
                                result.GetSolidPhaseMoleFraction,
                                result.GetSolidPhaseMoleFractions}

        End Function

        Public Overrides Function Flash_PV(Vz() As Double, P As Double, V As Double, Tref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object

            Dim result As FlashCalculationResult = PVFlash.Invoke(Vz, P, V, Tref, PP)

            Return New Object() {result.GetLiquidPhase1MoleFraction,
                                result.GetVaporPhaseMoleFraction,
                                result.GetLiquidPhase1MoleFractions,
                                result.GetVaporPhaseMoleFractions,
                                result.CalculatedTemperature,
                                result.IterationsTaken,
                                result.Kvalues,
                                result.GetLiquidPhase2MoleFraction,
                                result.GetLiquidPhase2MoleFractions,
                                result.GetSolidPhaseMoleFraction,
                                result.GetSolidPhaseMoleFractions}

        End Function

        Public Overrides Function Flash_TV(Vz() As Double, T As Double, V As Double, Pref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object

            Dim result As FlashCalculationResult = TVFlash.Invoke(Vz, T, V, Pref, PP)

            Return New Object() {result.GetLiquidPhase1MoleFraction,
                                result.GetVaporPhaseMoleFraction,
                                result.GetLiquidPhase1MoleFractions,
                                result.GetVaporPhaseMoleFractions,
                                result.CalculatedPressure,
                                result.IterationsTaken,
                                result.Kvalues,
                                result.GetLiquidPhase2MoleFraction,
                                result.GetLiquidPhase2MoleFractions,
                                result.GetSolidPhaseMoleFraction,
                                result.GetSolidPhaseMoleFractions}

        End Function

    End Class

End Namespace


