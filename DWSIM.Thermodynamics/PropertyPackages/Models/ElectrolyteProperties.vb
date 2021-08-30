'    Electrolyte-related Functions and Procedures
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

Imports System.Collections.Generic
Imports FileHelpers
Imports System.Math
Imports System.Linq
Imports System.Runtime.CompilerServices

Namespace PropertyPackages.Auxiliary

    <System.Serializable()> Public Class Electrolyte

        Function MIAC(Vxl As Double(), Vxs As Double(), T As Double, cprops As List(Of Interfaces.ICompoundConstantProperties)) As Double

            Dim n As Integer = Vxl.Length - 1
            Dim i As Integer
            Dim Im As Double

            Im = 0.0#
            For i = 0 To n
                Im += 0.5 * cprops(i).Charge ^ 2 * Vxl(i)
            Next

            Dim DCsolv, result1, result2 As Double

            DCsolv = 289.82 - 1.148 * T + 0.0017843 * T ^ 2 + -0.000001053 * T ^ 3

            result1 = -1824000.0 * Im ^ 0.5 / ((DCsolv * T) ^ (3 / 2))

            i = 0
            Dim sums As Double = 0
            Do
                If cprops(i).IsSalt Then
                    result2 += Vxs(i) * Math.Abs(cprops(i).PositiveIonStoichCoeff * cprops(i).NegativeIonStoichCoeff)
                    sums += Vxs(i)
                End If
                i += 1
            Loop Until i = n + 1

            Return Math.Exp(result1 * result2 / sums)

        End Function

        Function FreezingPointDepression(ByVal Vx As Double(), activcoeff As Double(), cprops As List(Of Interfaces.ICompoundConstantProperties)) As Double()

            Dim n As Integer = Vx.Length - 1
            Dim wid As Integer = cprops.IndexOf((From c As Interfaces.ICompoundConstantProperties In cprops Select c Where c.Name = "Water").SingleOrDefault)
            Dim Tnfp, DHm, DT, Td As Double

            Tnfp = cprops(wid).TemperatureOfFusion
            DHm = cprops(wid).EnthalpyOfFusionAtTf

            DT = 0.00831447 * Tnfp ^ 2 / DHm * Math.Log(Vx(wid) * activcoeff(wid))
            Td = Tnfp - DT

            Return New Double() {Td, DT}

        End Function

        Function OsmoticCoeff(Vx As Double(), activcoeff As Double(), cprops As List(Of Interfaces.ICompoundConstantProperties)) As Double

            Dim n As Integer = Vx.Length - 1
            Dim i As Integer
            Dim molality(n), summ As Double

            Dim wid As Integer = cprops.IndexOf((From c As Interfaces.ICompoundConstantProperties In cprops Select c Where c.Name = "Water").SingleOrDefault)

            'calculate molality considering 1 mol of mixture.

            Dim wtotal As Double = 0

            i = 0
            Do
                If cprops(i).Name = "Water" Then
                    wtotal += Vx(i) * cprops(i).Molar_Weight / 1000
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Do
                If cprops(i).IsIon Then
                    molality(i) = Vx(i) / wtotal
                    summ += molality(i)
                End If
                i += 1
            Loop Until i = n + 1

            Dim oc As Double = -Log(Vx(wid) * activcoeff(wid)) / (cprops(wid).Molar_Weight / 1000 * summ)

            Return oc

        End Function

        Function pH(Vx As Double(), T As Double, activcoeff As Double(), cprops As List(Of Interfaces.ICompoundConstantProperties)) As Double

            Dim n As Integer = Vx.Length - 1
            Dim i As Integer
            Dim molality(n), summ As Double

            Dim wid As Integer = cprops.IndexOf((From c As Interfaces.ICompoundConstantProperties In cprops Select c Where c.Name = "Water").SingleOrDefault)
            Dim hid As Integer = cprops.IndexOf((From c As Interfaces.ICompoundConstantProperties In cprops Select c Where c.Name = "Hydron").SingleOrDefault)

            'calculate molality considering 1 mol of mixture.

            Dim wtotal As Double = 0

            i = 0
            Do
                If cprops(i).Name = "Water" Then
                    wtotal += Vx(i) * cprops(i).Molar_Weight / 1000
                End If
                i += 1
            Loop Until i = n + 1

            Dim wden = LiquidDensity(Vx, T, cprops)

            i = 0
            Do
                If cprops(i).IsIon Then
                    molality(i) = Vx(i) / wtotal * wden / 1000
                    summ += molality(i)
                End If
                i += 1
            Loop Until i = n + 1

            Dim phvalue As Double = 0.0#

            If hid <> -1 Then phvalue = -Log10(molality(hid) * activcoeff(hid))

            Return phvalue

        End Function

        Function pH(Vx As Double(), T As Double, cprops As List(Of Interfaces.ICompoundConstantProperties)) As Double

            Dim n As Integer = Vx.Length - 1
            Dim i As Integer
            Dim molality(n), summ As Double

            Dim wid As Integer = cprops.IndexOf((From c As Interfaces.ICompoundConstantProperties In cprops Select c Where c.Name = "Water").SingleOrDefault)
            Dim hid As Integer = cprops.IndexOf((From c As Interfaces.ICompoundConstantProperties In cprops Select c Where c.Name = "Hydron").SingleOrDefault)

            'calculate molality considering 1 mol of mixture.

            Dim wtotal As Double = 0

            i = 0
            Do
                If cprops(i).Name = "Water" Then
                    wtotal += Vx(i) * cprops(i).Molar_Weight / 1000
                End If
                i += 1
            Loop Until i = n + 1

            Dim wden = LiquidDensity(Vx, T, cprops)

            i = 0
            Do
                If cprops(i).IsIon Then
                    molality(i) = Vx(i) / wtotal * wden / 1000
                    summ += molality(i)
                End If
                i += 1
            Loop Until i = n + 1

            Dim phvalue As Double = 0.0#

            If hid <> -1 Then phvalue = -Log10(molality(hid))

            Return phvalue

        End Function

        Function SolidEnthalpy(ByVal T As Double, ByVal Vx As Double(), cprops As List(Of Interfaces.ICompoundConstantProperties)) As Double

            Dim n As Integer = Vx.Length - 1
            Dim i As Integer
            Dim HS As Double = 0.0#
            Dim MW As Double = 0.0#

            For i = 0 To n
                HS += Vx(i) * cprops(i).Electrolyte_Cp0 * (T - 298)
                MW += Vx(i) * cprops(i).Molar_Weight
            Next

            If MW = 0.0 Then
                Return 0.0
            Else
                Return HS / MW 'kJ/kg
            End If

        End Function

        Function LiquidEnthalpy(ByVal T As Double, ByVal Vx As Double(), cprops As List(Of Interfaces.ICompoundConstantProperties), activcoeffT2 As Double(), activcoeffT1 As Double(), ExcessOnly As Boolean) As Double

            Dim T0, Cp0, CpT As Double

            Dim n As Integer = Vx.Length - 1

            T0 = 298.15
            Cp0 = HeatCapacityCp(T0, Vx, cprops)
            CpT = HeatCapacityCp(T, Vx, cprops)

            Dim H, Hex As Double
            Dim MW As Double = 0.0#

            H = (CpT + Cp0) / 2 * (T - T0)

            Dim wid As Integer = cprops.IndexOf((From c2 As Interfaces.ICompoundConstantProperties In cprops Select c2 Where c2.Name = "Water").SingleOrDefault)

            Hex = 0.0#
            MW = 1.0#

            If Vx(wid) <> 1.0# And Vx(wid) > 0.0# Then
                MW = 0.0#
                For i As Integer = 0 To n
                    If Vx(i) <> 0.0# Then
                        Hex += -8.314 * T ^ 2 * Vx(i) * (Log(activcoeffT2(i)) - Log(activcoeffT1(i))) / (0.1) 'kJ/kmol
                        MW += Vx(i) * cprops(i).Molar_Weight
                    End If
                Next
            End If

            If MW = 0.0 Then
                Return 0.0
            Else
                If ExcessOnly Then Return Hex / MW Else Return H + Hex / MW 'kJ/kg
            End If

        End Function

        Function HeatCapacityCp(ByVal T As Double, ByVal Vx As Double(), cprops As List(Of Interfaces.ICompoundConstantProperties)) As Double

            Dim wid As Integer = cprops.IndexOf((From c2 As Interfaces.ICompoundConstantProperties In cprops Select c2 Where c2.Name = "Water").SingleOrDefault)

            Dim n As Integer = Vx.Length - 1
            Dim i As Integer
            Dim Cp As Double = 0.0#
            Dim MW As Double = 0.0#
            Dim A, B, C, D, E, Cpi As Double

            For i = 0 To n
                If i = wid Then
                    A = cprops(i).Liquid_Heat_Capacity_Const_A
                    B = cprops(i).Liquid_Heat_Capacity_Const_B
                    C = cprops(i).Liquid_Heat_Capacity_Const_C
                    D = cprops(i).Liquid_Heat_Capacity_Const_D
                    E = cprops(i).Liquid_Heat_Capacity_Const_E
                    Cpi = (A + Exp(B / T + C + D * T + E * T ^ 2)) / 1000
                    Cp += Vx(i) * Cpi
                    MW += Vx(i) * cprops(i).Molar_Weight
                Else
                    Cpi = (cprops(i).Ion_CpAq_a + cprops(i).Ion_CpAq_b * T + cprops(i).Ion_CpAq_c / (T - 200))
                    Cp += Vx(i) * Cpi
                    MW += Vx(i) * cprops(i).Molar_Weight
                End If
            Next

            If MW = 0.0 Then Return 0.0 Else Return Cp / MW 'kJ/kg

        End Function

        Function LiquidDensity(Vx As Double(), T As Double, cprops As List(Of Interfaces.ICompoundConstantProperties)) As Double

            Dim wid As Integer = cprops.IndexOf((From c As Interfaces.ICompoundConstantProperties In cprops Select c Where c.Name = "Water").SingleOrDefault)

            Dim n As Integer = Vx.Length - 1
            Dim molality(n) As Double
            Dim i As Integer
            Dim vol As Double = 0.0#
            Dim MW As Double = 0.0#
            Dim Tr As Double = 0

            Dim wtotal As Double = 0

            i = 0
            Do
                If cprops(i).Name = "Water" Then
                    wtotal += Vx(i) * cprops(i).Molar_Weight / 1000
                End If
                i += 1
            Loop Until i = n + 1

            Dim Xsolv As Double = 1

            i = 0
            Do
                molality(i) = Vx(i) / wtotal
                'salt-free mole fractions
                If cprops(i).Name <> "Water" Then
                    Xsolv -= Vx(i)
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Dim Im As Double = 0.0#
            Do
                Im += cprops(i).Charge ^ 2 * molality(i) / 2
                i += 1
            Loop Until i = n + 1

            vol = 0.0#
            For i = 0 To n
                If i = wid Then
                    Tr = T / cprops(i).Critical_Temperature
                    vol += Vx(i) * (1 / (32.51621 * (1 - Tr) ^ (-3.213004 + 7.92411 * Tr + -7.359898 * Tr ^ 2 + 2.703522 * Tr ^ 3))) 'm3/kmol
                ElseIf cprops(i).IsIon Then
                    With cprops(i)
                        vol += Vx(i) * ((.StandardStateMolarVolume + .MolarVolume_v2i * (T - 298.15) + .MolarVolume_v3i * (T - 298.15) ^ 2) +
                                (.MolarVolume_k1i + .MolarVolume_k2i * (T - 298.15) + .MolarVolume_k3i * (T - 298.15) ^ 2) * Im ^ 0.5) * 0.001 'm3/kmol
                    End With
                Else
                    vol += 0.0#
                End If
                MW += Vx(i) * cprops(i).Molar_Weight
            Next

            If MW = 0.0 Then Return 0.0 Else Return MW / vol 'kJ/kg

        End Function

    End Class

End Namespace
