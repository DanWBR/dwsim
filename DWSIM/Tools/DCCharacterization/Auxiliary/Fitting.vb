'    Petroleum Fractions Property Calculation Routines
'    Copyright 2010 Daniel Wagner O. de Medeiros
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
Imports DWSIM.DWSIM.MathEx.BrentOpt
Imports DWSIM.DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.DWSIM.SimulationObjects.PropertyPackages.Auxiliary
Imports DWSIM.DWSIM.SimulationObjects.PropertyPackages
Imports DWSIM.DWSIM.SimulationObjects.Streams

Namespace DWSIM.Utilities.PetroleumCharacterization.Methods

    Public Class DensityFitting

        Public _comp As Compound

        Public Function FunctionValue(ByVal t As Double) As Double
            Dim result As Double = 0
            result = CalcDensityDiff(15.6 + 273.15, t, _comp) ^ 2
            Return result
        End Function

        Public Function MinimizeError() As Double

            Dim brentsolver As New BrentMinimize
            brentsolver.DefineFuncDelegate(AddressOf FunctionValue)

            Dim factor, fval As Double

            fval = brentsolver.brentoptimize(0.1, 10, 0.01, factor)

            Return factor

        End Function

        Public Function CalcDensityDiff(ByVal T As Double, ByVal multipl As Double, ByVal comp As Compound) As Double

            Dim dens1, dens2 As Double

            dens1 = comp.ConstantProperties.PF_SG * 999.96

            With comp.ConstantProperties
                dens2 = DWSIM.SimulationObjects.PropertyPackages.Auxiliary.PROPS.liq_dens_rackett(T, .Critical_Temperature, .Critical_Pressure, .Acentric_Factor, .Molar_Weight, .Z_Rackett * multipl)
            End With

            If Double.IsNaN(dens1 - dens2) Then
                Return 0
            Else
                Return dens1 - dens2
            End If

        End Function

    End Class

    Public Class NBPFitting

        Public _ms As MaterialStream
        Public _pp As PropertyPackage
        Public _idx As Integer

        Public Function FunctionValue(ByVal t As Double) As Double
            Dim result As Double = 0
            Dim i As Integer = 0
            For Each c As Compound In _ms.Phases(0).Componentes.Values
                If i = _idx Then
                    c.ConstantProperties.Acentric_Factor *= t
                    result = CalcNBPDiff(i, 101325, t, c) ^ 2
                End If
                i += 1
            Next
            i = 0
            For Each c As Compound In _ms.Phases(0).Componentes.Values
                If i = _idx Then
                    c.ConstantProperties.Acentric_Factor /= t
                End If
                i += 1
            Next
            Return result
        End Function

        Public Function MinimizeError() As Double

            Dim brentsolver As New BrentMinimize
            brentsolver.DefineFuncDelegate(AddressOf FunctionValue)

            Dim factor, fval As Double

            fval = brentsolver.brentoptimize(0.001, 10, 0.1, factor)

            Return factor

        End Function

        Public Function CalcNBPDiff(ByVal index As Integer, ByVal P As Double, ByVal multipl As Double, ByVal comp As Compound) As Double

            Dim NBP1, NBP2 As Double

            Dim Vx(_ms.Phases(0).Componentes.Count - 1) As Double

            Vx(index) = 1.0#

            NBP1 = comp.ConstantProperties.NBP

            _pp.CurrentMaterialStream = _ms

            NBP2 = _pp.DW_CalcBubT(Vx, P)(4)

            If Double.IsNaN(NBP1 - NBP2) Then
                Return 0
            Else
                Return NBP2 - NBP1
            End If

        End Function

    End Class

    Public Class PRVSFitting

        Public _comp As Compound

        Sub New()
        End Sub

        Public Function FunctionValue(ByVal t As Double) As Double
            Dim result As Double = 0
            result = CalcDensityDiff(15.6 + 273.15, 101325, t, _comp) ^ 2
            Return result
        End Function

        Public Function MinimizeError() As Double

            Dim brentsolver As New BrentMinimize
            brentsolver.DefineFuncDelegate(AddressOf FunctionValue)

            Dim factor, fval As Double

            fval = brentsolver.brentoptimize(-100, 100, 0.0001, factor)

            Return factor

        End Function

        Public Function CalcDensityDiff(ByVal T As Double, ByVal P As Double, ByVal multipl As Double, ByVal comp As Compound) As Double

            Dim dens1, dens2 As Double
            Dim pr As New PengRobinson()

            dens1 = comp.ConstantProperties.PF_SG * 999.96

            With comp.ConstantProperties
                Dim val As Double
                val = pr.Z_PR(T, P, New Double() {1.0#}, New Double(,) {{0.0#}, {0.0#}}, New Double() {.Critical_Temperature}, New Double() {.Critical_Pressure}, New Double() {.Acentric_Factor}, "L")
                val = (8.314 * val * T / P)
                val -= .PR_Volume_Translation_Coefficient * multipl * pr.bi(0.0778, .Critical_Temperature, .Critical_Pressure)
                val = 1 / val * .Molar_Weight / 1000
                dens2 = val
            End With

            If Double.IsNaN(dens1 - dens2) Then
                Return 0
            Else
                Return dens1 - dens2
            End If

        End Function

    End Class

    Public Class SRKVSFitting

        Public _comp As Compound

        Sub New()
        End Sub

        Public Function FunctionValue(ByVal t As Double) As Double
            Dim result As Double = 0
            result = CalcDensityDiff(15.6 + 273.15, 101325, t, _comp) ^ 2
            Return result
        End Function

        Public Function MinimizeError() As Double

            Dim brentsolver As New BrentMinimize
            brentsolver.DefineFuncDelegate(AddressOf FunctionValue)

            Dim factor, fval As Double

            fval = brentsolver.brentoptimize(-100, 100, 0.0001, factor)

            Return factor

        End Function

        Public Function CalcDensityDiff(ByVal T As Double, ByVal P As Double, ByVal multipl As Double, ByVal comp As Compound) As Double

            Dim dens1, dens2 As Double
            Dim srk As New SRK()

            dens1 = comp.ConstantProperties.PF_SG * 999.96

            With comp.ConstantProperties
                Dim val As Double
                val = srk.Z_SRK(T, P, New Double() {1.0#}, New Double(,) {{0.0#}, {0.0#}}, New Double() {.Critical_Temperature}, New Double() {.Critical_Pressure}, New Double() {.Acentric_Factor}, "L")
                val = (8.314 * val * T / P)
                val -= .SRK_Volume_Translation_Coefficient * multipl * srk.bi(0.0778, .Critical_Temperature, .Critical_Pressure)
                val = 1 / val * .Molar_Weight / 1000
                dens2 = val
            End With

            If Double.IsNaN(dens1 - dens2) Then
                Return 0
            Else
                Return dens1 - dens2
            End If

        End Function

    End Class

End Namespace

