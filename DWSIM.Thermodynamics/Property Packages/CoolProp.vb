'    CoolProp Property Package
'    Copyright 2014 Daniel Wagner O. de Medeiros
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

Imports DWSIM.Thermodynamics.PropertyPackages
Imports System.Math
Imports DWSIM.MathOps.MathEx

Imports System.Runtime.InteropServices
Imports CoolProp
Imports System.Linq

Namespace PropertyPackages


    <System.Runtime.InteropServices.Guid(CoolPropPropertyPackage.ClassId)> _
    <System.Serializable()> Public Class CoolPropPropertyPackage

        Inherits PropertyPackages.PropertyPackage

        Public Shadows Const ClassId As String = "1F5B0263-E936-40d5-BA5B-FFAB11595E43"

        Public Sub New(ByVal comode As Boolean)
            MyBase.New(comode)
        End Sub

        Public Sub New()

            MyBase.New()

            With Me.Parameters
                .Item("PP_IDEAL_MIXRULE_LIQDENS") = 1
                .Item("PP_USEEXPLIQDENS") = 1
            End With

            Me.IsConfigurable = True
            Me._packagetype = PropertyPackages.PackageType.Miscelaneous

        End Sub

        Public Overrides Sub ReconfigureConfigForm()
            MyBase.ReconfigureConfigForm()
        End Sub

#Region "    DWSIM Functions"

        Private Sub WriteWarningMessage(message As String)
            Select Case Settings.DebugLevel
                Case 0
                    'do nothing
                Case 1
                    Console.WriteLine(message)
                Case 2, 3

            End Select
        End Sub

        Public Overrides Function AUX_CPi(sub1 As String, T As Double) As Double
            Dim Tmin, Tmax, Tc, val As Double
            If IsCompoundSupported(sub1) Then
                Tmin = CoolProp.Props1SI(sub1, "TMIN")
                Tmax = CoolProp.Props1SI(sub1, "TMAX")
                Tc = CoolProp.Props1SI(sub1, "TCRIT")
                If T > Tmin And T <= Tmax Then
                    Try
                        If T <= Tc Then
                            val = CoolProp.PropsSI("CP0MASS", "T", T, "Q", 1, sub1) / 1000
                        Else
                            val = CoolProp.PropsSI("CP0MASS", "T", T, "P", 101325, sub1) / 1000
                        End If
                    Catch ex As Exception
                        WriteWarningMessage(ex.Message.ToString & ". Estimating value... [Fluid: " & sub1 & "]")
                        val = MyBase.AUX_CPi(sub1, T)
                    End Try
                Else
                    WriteWarningMessage("CoolProp Warning: unable to calculate Ideal Gas Heat Capacity for " & sub1 & " at T = " & T & " K. Estimating value...")
                    val = MyBase.AUX_CPi(sub1, T)
                End If
            Else
                WriteWarningMessage("CoolProp Warning: compound " & sub1 & " not supported. Estimating Ideal Gas Heat Capacity value...")
                val = MyBase.AUX_CPi(sub1, T)
            End If
            Return val
        End Function

        Public Overrides Function AUX_PVAPi(index As Integer, T As Double) As Double
            Calculator.CheckParallelPInvoke()
            Dim sub1 As String = RET_VNAMES()(index)
            Dim Tmin, Tmax, Tc, val As Double
            If IsCompoundSupported(sub1) Then
                Tmin = CoolProp.Props1SI(sub1, "TMIN")
                Tmax = CoolProp.Props1SI(sub1, "TMAX")
                Tc = CoolProp.Props1SI(sub1, "TCRIT")
                If T > Tmin And T <= Tmax And T <= Tc Then
                    Try
                        val = CoolProp.PropsSI("P", "T", T, "Q", 0, sub1)
                    Catch ex As Exception
                        WriteWarningMessage(ex.Message.ToString & ". Estimating value... [Fluid: " & sub1 & "]")
                        val = MyBase.AUX_PVAPi(index, T)
                    End Try
                Else
                    val = MyBase.AUX_PVAPi(index, T)
                End If
            Else
                WriteWarningMessage("CoolProp Warning: compound " & sub1 & " not supported. Estimating Vapor Pressure value...")
                val = MyBase.AUX_PVAPi(index, T)
            End If
            Return val
        End Function

        Public Overrides Function AUX_PVAPi(sub1 As String, T As Double) As Object
            Calculator.CheckParallelPInvoke()
            Dim Tmin, Tmax, Tc, val As Double
            If IsCompoundSupported(sub1) Then
                Tmin = CoolProp.Props1SI(sub1, "TMIN")
                Tc = CoolProp.Props1SI(sub1, "TCRIT")
                If T > Tmin And T <= Tmax And T <= Tc Then
                    Try
                        val = CoolProp.PropsSI("P", "T", T, "Q", 0, sub1)
                    Catch ex As Exception
                        WriteWarningMessage(ex.Message.ToString & ". Estimating value... [Fluid: " & sub1 & "]")
                        val = MyBase.AUX_PVAPi(sub1, T)
                    End Try
                Else
                    val = MyBase.AUX_PVAPi(sub1, T)
                End If
            Else
                WriteWarningMessage("CoolProp Warning: compound " & sub1 & " not supported. Estimating Vapor Pressure value...")
                val = MyBase.AUX_PVAPi(sub1, T)
            End If
            Return val
        End Function

        Public Overrides Function AUX_TSATi(PVAP As Double, index As Integer) As Double
            Dim sub1 As String = RET_VNAMES()(index)
            Dim Pmin, Pmax, val As Double
            If IsCompoundSupported(sub1) Then
                Pmin = CoolProp.Props1SI(sub1, "PMIN")
                Pmax = CoolProp.Props1SI(sub1, "PMAX")
                If PVAP > Pmin And PVAP < Pmax Then
                    Try
                        val = CoolProp.PropsSI("T", "P", PVAP, "Q", 0, sub1)
                    Catch ex As Exception
                        WriteWarningMessage(ex.Message.ToString & ". Estimating value... [Fluid: " & sub1 & "]")
                        val = MyBase.AUX_TSATi(PVAP, index)
                    End Try
                Else
                    WriteWarningMessage("CoolProp Warning: unable to calculate Saturation Temperature for " & sub1 & " at P = " & PVAP & " Pa. Estimating value...")
                    val = MyBase.AUX_TSATi(PVAP, index)
                End If
            Else
                WriteWarningMessage("CoolProp Warning: compound " & sub1 & " not supported. Estimating Saturation Temperature value...")
                val = MyBase.AUX_TSATi(PVAP, index)
            End If
            Return val
        End Function

        Public Overrides Function AUX_TSATi(PVAP As Double, subst As String) As Double
            Dim Pmin, Pmax, val As Double
            If IsCompoundSupported(subst) Then
                Pmin = CoolProp.Props1SI(subst, "PMIN")
                Pmax = CoolProp.Props1SI(subst, "PMAX")
                If PVAP > Pmin And PVAP < Pmax Then
                    Try
                        val = CoolProp.PropsSI("T", "P", PVAP, "Q", 0, subst)
                    Catch ex As Exception
                        WriteWarningMessage(ex.Message.ToString & ". Estimating value... [Fluid: " & subst & "]")
                        val = MyBase.AUX_TSATi(PVAP, subst)
                    End Try
                Else
                    WriteWarningMessage("CoolProp Warning: unable to calculate Saturation Temperature for " & subst & " at P = " & PVAP & " Pa. Estimating value...")
                    val = MyBase.AUX_TSATi(PVAP, subst)
                End If
            Else
                WriteWarningMessage("CoolProp Warning: compound " & subst & " not supported. Estimating Saturation Temperature value...")
                val = MyBase.AUX_TSATi(PVAP, subst)
            End If
            Return val
        End Function

        Public Overrides Function AUX_LIQDENSi(cprop As Interfaces.ICompoundConstantProperties, T As Double) As Double
            Dim sub1 = cprop.Name
            Dim Tmin, Tmax, Tc, val As Double
            If IsCompoundSupported(sub1) Then
                Tmin = CoolProp.Props1SI(sub1, "TMIN")
                Tmax = CoolProp.Props1SI(sub1, "TMAX")
                Tc = CoolProp.Props1SI(sub1, "TCRIT")
                If T > Tmin And T <= Tmax And T <= Tc Then
                    Try
                        val = CoolProp.PropsSI("D", "T", T, "Q", 0, cprop.Name)
                    Catch ex As Exception
                        WriteWarningMessage(ex.Message.ToString & ". Estimating value... [Fluid: " & sub1 & "]")
                        val = MyBase.AUX_LIQDENSi(cprop, T)
                    End Try
                Else
                    WriteWarningMessage("CoolProp Warning: unable to calculate Liquid Density for " & sub1 & " at T = " & T & " K. Estimating value...")
                    val = MyBase.AUX_LIQDENSi(cprop, T)
                End If
            Else
                WriteWarningMessage("CoolProp Warning: compound " & sub1 & " not supported. Estimating Liquid Density value...")
                val = MyBase.AUX_LIQDENSi(cprop, T)
            End If
            Return val
        End Function

        Public Overrides Function AUX_LIQ_Cpi(cprop As Interfaces.ICompoundConstantProperties, T As Double) As Double
            Dim sub1 = cprop.Name
            Dim Tmin, Tmax, Tc, val As Double
            If IsCompoundSupported(sub1) Then
                Tmin = CoolProp.Props1SI(sub1, "TMIN")
                Tmax = CoolProp.Props1SI(sub1, "TMAX")
                Tc = CoolProp.Props1SI(sub1, "TCRIT")
                If T > Tmin And T <= Tmax And T <= Tc Then
                    Try
                        val = CoolProp.PropsSI("C", "T", T, "Q", 0, cprop.Name) / 1000
                    Catch ex As Exception
                        WriteWarningMessage(ex.Message.ToString & ". Estimating value... [Fluid: " & sub1 & "]")
                        val = MyBase.AUX_LIQ_Cpi(cprop, T)
                    End Try
                Else
                    WriteWarningMessage("CoolProp Warning: unable to calculate Liquid Heat Capacity for " & sub1 & " at T = " & T & " K. Estimating value...")
                    val = MyBase.AUX_LIQ_Cpi(cprop, T)
                End If
            Else
                WriteWarningMessage("CoolProp Warning: compound " & sub1 & " not supported. Estimating Liquid Heat Capacity value...")
                val = MyBase.AUX_LIQ_Cpi(cprop, T)
            End If
            Return val
        End Function

        Public Overrides Function AUX_CONDTG(T As Double, P As Double) As Double
            Dim val As Double
            Dim i As Integer
            Dim Tmin, Tmax, Pmin, Pmax, Tb, Tc As Double
            Dim vk(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            i = 0
            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                If IsCompoundSupported(subst.Name) Then
                    Tmin = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMIN")
                    Tmax = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMAX")
                    Pmin = CoolProp.Props1SI(subst.ConstantProperties.Name, "PMIN")
                    Pmax = CoolProp.Props1SI(subst.ConstantProperties.Name, "PMAX")
                    Tc = CoolProp.Props1SI(subst.ConstantProperties.Name, "TCRIT")
                    If T > Tmin And T < Tmax And P > Pmin And P < Pmax Then
                        If T < Tc Then Tb = CoolProp.PropsSI("T", "P", P, "Q", 1, subst.ConstantProperties.Name) Else Tb = Tc
                        If T > Tb Then
                            Try
                                vk(i) = CoolProp.PropsSI("L", "T", T, "P", P, subst.ConstantProperties.Name)
                            Catch ex As Exception
                                WriteWarningMessage(ex.Message.ToString & ". Estimating value using Ely-Hanley [Fluid: " & subst.ConstantProperties.Name & "]")
                                vk(i) = Auxiliary.PROPS.condtg_elyhanley(T, subst.ConstantProperties.Critical_Temperature,
                                                               subst.ConstantProperties.Critical_Volume / 1000, subst.ConstantProperties.Critical_Compressibility,
                                                               subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight,
                                                               Me.AUX_CPi(subst.ConstantProperties.Name, T) * subst.ConstantProperties.Molar_Weight - 8.314)
                            End Try
                        Else
                            Try
                                WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Vapor Thermal Conductivity, compound " &
                                                    subst.ConstantProperties.Name & ". Extrapolating curve to obtain a value...")
                                Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                x1 = Tb + (Tmax - Tb) * 0.2
                                x2 = Tb + (Tmax - Tb) * 0.4
                                x3 = Tb + (Tmax - Tb) * 0.6
                                x4 = Tb + (Tmax - Tb) * 0.8
                                x5 = Tb + (Tmax - Tb) * 0.9
                                p1 = CoolProp.PropsSI("L", "T", x1, "P", P, subst.ConstantProperties.Name)
                                p2 = CoolProp.PropsSI("L", "T", x2, "P", P, subst.ConstantProperties.Name)
                                p3 = CoolProp.PropsSI("L", "T", x3, "P", P, subst.ConstantProperties.Name)
                                p4 = CoolProp.PropsSI("L", "T", x4, "P", P, subst.ConstantProperties.Name)
                                p5 = CoolProp.PropsSI("L", "T", x5, "P", P, subst.ConstantProperties.Name)
                                vk(i) = Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                            Catch ex As Exception
                                WriteWarningMessage(ex.Message.ToString & ". Estimating value using Ely-Hanley [Fluid: " & subst.ConstantProperties.Name & "]")
                                vk(i) = Auxiliary.PROPS.condtg_elyhanley(T, subst.ConstantProperties.Critical_Temperature,
                                                               subst.ConstantProperties.Critical_Volume / 1000, subst.ConstantProperties.Critical_Compressibility,
                                                               subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight,
                                                               Me.AUX_CPi(subst.ConstantProperties.Name, T) * subst.ConstantProperties.Molar_Weight - 8.314)
                            End Try
                        End If
                    Else
                        WriteWarningMessage("CoolProp Warning: unable to calculate Vapor Phase Thermal Conductivity for " &
                                                                              subst.ConstantProperties.Name & " at T = " & T & " K and P = " & P &
                                                                              " Pa. Estimating value using Ely-Hanley...")
                        vk(i) = Auxiliary.PROPS.condtg_elyhanley(T, subst.ConstantProperties.Critical_Temperature,
                                                       subst.ConstantProperties.Critical_Volume / 1000, subst.ConstantProperties.Critical_Compressibility,
                                                       subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight,
                                                       Me.AUX_CPi(subst.ConstantProperties.Name, T) * subst.ConstantProperties.Molar_Weight - 8.314)
                    End If
                Else
                    WriteWarningMessage("CoolProp Warning: compound " & subst.ConstantProperties.Name & " not supported. Estimating Vapor Thermal Conductivity with Ely-Hanley method...")
                    vk(i) = Auxiliary.PROPS.condtg_elyhanley(T, subst.ConstantProperties.Critical_Temperature,
                                                   subst.ConstantProperties.Critical_Volume / 1000, subst.ConstantProperties.Critical_Compressibility,
                                                   subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight,
                                                   Me.AUX_CPi(subst.ConstantProperties.Name, T) * subst.ConstantProperties.Molar_Weight - 8.314)
                End If
                If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                vk(i) = subst.MassFraction * vk(i)
                i = i + 1
            Next
            val = MathEx.Common.Sum(vk)
            Return val
        End Function

        Public Overrides Function AUX_CONDTL(T As Double, Optional phaseid As Integer = 3) As Double
            Dim val As Double
            Dim i As Integer
            Dim Tmin, Tmax, Tb, Tc As Double
            Dim vk(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            i = 0
            Dim P As Double = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault
            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(phaseid).Compounds.Values
                If IsCompoundSupported(subst.Name) Then
                    Tmin = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMIN")
                    Tmax = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMAX")
                    Tc = CoolProp.Props1SI(subst.ConstantProperties.Name, "TCRIT")
                    If T > Tmin And T <= Tmax And T <= Tc Then
                        Try
                            Tb = Me.AUX_TSATi(P, i)
                            If T < Tb Then
                                vk(i) = CoolProp.PropsSI("L", "T", T, "Q", 0, subst.ConstantProperties.Name)
                            Else
                                WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Liquid Thermal Conductivity, compound " &
                          subst.ConstantProperties.Name & ". Extrapolating curve to obtain a value...")
                                Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                x1 = Tb * 0.98
                                x2 = Tb * 0.94
                                x3 = Tb * 0.92
                                x4 = Tb * 0.86
                                x5 = Tb * 0.8
                                p1 = CoolProp.PropsSI("L", "T", x1, "P", P, subst.ConstantProperties.Name)
                                p2 = CoolProp.PropsSI("L", "T", x2, "P", P, subst.ConstantProperties.Name)
                                p3 = CoolProp.PropsSI("L", "T", x3, "P", P, subst.ConstantProperties.Name)
                                p4 = CoolProp.PropsSI("L", "T", x4, "P", P, subst.ConstantProperties.Name)
                                p5 = CoolProp.PropsSI("L", "T", x5, "P", P, subst.ConstantProperties.Name)
                                vk(i) = Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                            End If
                        Catch ex As Exception
                            WriteWarningMessage(ex.Message.ToString & ". Estimating value using Latini [Fluid: " & subst.ConstantProperties.Name & "]")
                            vk(i) = Auxiliary.PROPS.condl_latini(T, subst.ConstantProperties.Normal_Boiling_Point, subst.ConstantProperties.Critical_Temperature,
                                                       subst.ConstantProperties.Molar_Weight, "X")
                        End Try
                    Else
                        WriteWarningMessage("CoolProp Warning: unable to calculate Liquid Phase Thermal Conductivity for " &
                                                                              subst.ConstantProperties.Name & " at T = " & T & " K. Estimating value using Latini...")
                        vk(i) = Auxiliary.PROPS.condl_latini(T, subst.ConstantProperties.Normal_Boiling_Point, subst.ConstantProperties.Critical_Temperature,
                                                   subst.ConstantProperties.Molar_Weight, "X")
                    End If
                Else
                    WriteWarningMessage("CoolProp Warning: compound " & subst.ConstantProperties.Name & " not supported. Estimating Liquid Thermal Conductivity with Latini method...")
                    vk(i) = Auxiliary.PROPS.condl_latini(T, subst.ConstantProperties.Normal_Boiling_Point, subst.ConstantProperties.Critical_Temperature,
                                               subst.ConstantProperties.Molar_Weight, "X")
                End If
                If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                vk(i) = subst.MassFraction * vk(i)
                i = i + 1
            Next
            val = MathEx.Common.Sum(vk)
            Return val
        End Function

        Public Overrides Function AUX_LIQDENS(T As Double, Vx As System.Array, Optional P As Double = 0.0, Optional Pvp As Double = 0.0, Optional FORCE_EOS As Boolean = False) As Double
            Dim val As Double
            Dim i As Integer
            Dim Tmin, Tmax, Tc As Double
            Dim vk(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            i = 0
            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(1).Compounds.Values
                If IsCompoundSupported(subst.Name) Then
                    Tmin = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMIN")
                    Tmax = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMAX")
                    Tc = CoolProp.Props1SI(subst.ConstantProperties.Name, "TCRIT")
                    If T > Tmin And T <= Tmax And T <= Tc Then
                        Try
                            vk(i) = CoolProp.PropsSI("D", "T", T, "Q", 0, subst.ConstantProperties.Name)
                        Catch ex As Exception
                            WriteWarningMessage(ex.Message.ToString & ". Estimating value using Rackett [Fluid: " & subst.ConstantProperties.Name & "]")
                            vk(i) = Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure,
                                                           subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight)
                        End Try
                    Else
                        WriteWarningMessage("CoolProp Warning: unable to calculate Liquid Phase Density for " &
                                                                              subst.ConstantProperties.Name & " at T = " & T & " K and P = " & P &
                                                                              " Pa. Estimating value using Rackett...")
                        vk(i) = Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure,
                                                       subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight)
                    End If
                Else
                    WriteWarningMessage("CoolProp Warning: compound " & subst.ConstantProperties.Name & " not supported. Estimating Liquid Phase Density with Rackett method...")
                    vk(i) = Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure,
                                                   subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight)
                End If
                If Vx(i) <> 0.0# Then vk(i) = Vx(i) / vk(i)
                If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                i = i + 1
            Next
            val = 1 / MathEx.Common.Sum(vk)
            Return val
        End Function

        Public Overrides Function AUX_LIQDENSi(subst As Interfaces.ICompound, T As Double) As Double
            Dim sub1 = subst.ConstantProperties.Name
            Dim Tmin, Tmax, Tc, val As Double
            If IsCompoundSupported(sub1) Then
                Tmin = CoolProp.Props1SI(sub1, "TMIN")
                Tmax = CoolProp.Props1SI(sub1, "TMAX")
                Tc = CoolProp.Props1SI(subst.ConstantProperties.Name, "TCRIT")
                If T > Tmin And T <= Tmax And T <= Tc Then
                    Try
                        val = CoolProp.PropsSI("D", "T", T, "Q", 0, sub1) / 1000
                    Catch ex As Exception
                        WriteWarningMessage(ex.Message.ToString & ". Estimating value... [Fluid: " & sub1 & "]")
                        val = MyBase.AUX_LIQDENSi(subst, T)
                    End Try
                Else
                    WriteWarningMessage("CoolProp Warning: unable to calculate Liquid Density for " & sub1 & " at T = " & T & " K. Estimating value...")
                    val = MyBase.AUX_LIQDENSi(subst, T)
                End If
            Else
                WriteWarningMessage("CoolProp Warning: compound " & sub1 & " not supported. Estimating Liquid Density value...")
                val = MyBase.AUX_LIQDENSi(subst, T)
            End If
            Return val
        End Function

        Public Overrides Function AUX_LIQTHERMCONDi(cprop As Interfaces.ICompoundConstantProperties, T As Double) As Double
            Dim sub1 = cprop.Name
            Dim Tmin, Tmax, Tc, val As Double
            If IsCompoundSupported(sub1) Then
                Tmin = CoolProp.Props1SI(sub1, "TMIN")
                Tmax = CoolProp.Props1SI(sub1, "TMAX")
                Tc = CoolProp.Props1SI(sub1, "TCRIT")
                If T > Tmin And T <= Tmax And T <= Tc Then
                    Try
                        val = CoolProp.PropsSI("L", "T", T, "Q", 0, cprop.Name) * 1000
                    Catch ex As Exception
                        WriteWarningMessage(ex.Message.ToString & ". Estimating value... [Fluid: " & sub1 & "]")
                        val = MyBase.AUX_LIQTHERMCONDi(cprop, T)
                    End Try
                Else
                    WriteWarningMessage("CoolProp Warning: unable to calculate Liquid Thermal Conductivity for " & sub1 & " at T = " & T & " K. Estimating value...")
                    val = MyBase.AUX_LIQTHERMCONDi(cprop, T)
                End If
            Else
                WriteWarningMessage("CoolProp Warning: compound " & sub1 & " not supported. Estimating Liquid Thermal Conductivity value...")
                val = MyBase.AUX_LIQTHERMCONDi(cprop, T)
            End If
            Return val
        End Function

        Public Overrides Function AUX_LIQVISCi(sub1 As String, T As Double) As Object
            Dim Tmin, Tmax, Tc, val As Double
            If IsCompoundSupported(sub1) Then
                Tmin = CoolProp.Props1SI(sub1, "TMIN")
                Tmax = CoolProp.Props1SI(sub1, "TMAX")
                Tc = CoolProp.Props1SI(sub1, "TCRIT")
                If T > Tmin And T <= Tmax And T <= Tc Then
                    Try
                        val = CoolProp.PropsSI("V", "T", T, "Q", 0, sub1)
                    Catch ex As Exception
                        WriteWarningMessage(ex.Message.ToString & ". Estimating value... [Fluid: " & sub1 & "]")
                        val = MyBase.AUX_LIQVISCi(sub1, T)
                    End Try
                Else
                    WriteWarningMessage("CoolProp Warning: unable to calculate Liquid Viscosity for " & sub1 & " at T = " & T & " K. Estimating value...")
                    val = MyBase.AUX_LIQVISCi(sub1, T)
                End If
            Else
                WriteWarningMessage("CoolProp Warning: compound " & sub1 & " not supported. Estimating Liquid Viscosity value...")
                val = MyBase.AUX_LIQVISCi(sub1, T)
            End If
            Return val
        End Function

        Public Overrides Function AUX_SURFTi(constprop As Interfaces.ICompoundConstantProperties, T As Double) As Double
            Dim sub1 = constprop.Name
            Dim Tmin, Tmax, Tc, val As Double
            If IsCompoundSupported(sub1) Then
                Tmin = CoolProp.Props1SI(sub1, "TMIN")
                Tmax = CoolProp.Props1SI(sub1, "TMAX")
                Tc = CoolProp.Props1SI(sub1, "TCRIT")
                If T > Tmin And T <= Tmax Then
                    Try
                        If T <= Tc Then
                            val = CoolProp.PropsSI("I", "T", T, "Q", 0, sub1)
                        Else
                            val = CoolProp.PropsSI("I", "T", T, "P", 101325, sub1)
                        End If
                    Catch ex As Exception
                        WriteWarningMessage(ex.Message.ToString & ". Estimating value... [Fluid: " & sub1 & "]")
                        val = MyBase.AUX_LIQTHERMCONDi(constprop, T)
                    End Try
                Else
                    WriteWarningMessage("CoolProp Warning: unable to calculate Liquid Surface Tension for " & sub1 & " at T = " & T & " K. Estimating value...")
                    val = MyBase.AUX_LIQTHERMCONDi(constprop, T)
                End If
            Else
                WriteWarningMessage("CoolProp Warning: compound " & sub1 & " not supported. Estimating Liquid Thermal Surface Tension value...")
                val = MyBase.AUX_LIQTHERMCONDi(constprop, T)
            End If
            Return val
        End Function

        Public Overrides Function AUX_SURFTM(T As Double) As Double

            Dim subst As Interfaces.ICompound
            Dim val As Double = 0
            For Each subst In Me.CurrentMaterialStream.Phases(1).Compounds.Values
                val += subst.MoleFraction.GetValueOrDefault * Me.AUX_SURFTi(subst.ConstantProperties, T)
            Next
            Return val

        End Function

        Public Overrides Function AUX_VAPTHERMCONDi(cprop As Interfaces.ICompoundConstantProperties, T As Double, P As Double) As Double
            Dim sub1 = cprop.Name
            Dim Tmin, Tmax, Pmin, Pmax, Tb, Tc, val As Double
            If IsCompoundSupported(sub1) Then
                Tmin = CoolProp.Props1SI(cprop.Name, "TMIN")
                Tmax = CoolProp.Props1SI(cprop.Name, "TMAX")
                Pmin = CoolProp.Props1SI(cprop.Name, "PMIN")
                Pmax = CoolProp.Props1SI(cprop.Name, "PMAX")
                If T > Tmin And T < Tmax And P > Pmin And P < Pmax Then
                    Tc = CoolProp.Props1SI(cprop.Name, "TCRIT")
                    If T < Tc Then Tb = CoolProp.PropsSI("T", "P", P, "Q", 1, cprop.Name) Else Tb = Tc
                    If T > Tb Then
                        Try
                            val = CoolProp.PropsSI("L", "T", T, "P", P, cprop.Name) * 1000
                        Catch ex As Exception
                            WriteWarningMessage(ex.Message.ToString & ". Estimating value using Ely-Hanley [Fluid: " & cprop.Name & "]")
                            val = Auxiliary.PROPS.condtg_elyhanley(T, cprop.Critical_Temperature,
                                                           cprop.Critical_Volume / 1000, cprop.Critical_Compressibility,
                                                           cprop.Acentric_Factor, cprop.Molar_Weight,
                                                           Me.AUX_CPi(cprop.Name, T) * cprop.Molar_Weight - 8.314)
                        End Try
                    Else
                        Try
                            WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Vapor Thermal Conductivity, compound " &
                             cprop.Name & ". Extrapolating curve to obtain a value...")
                            Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                            x1 = Tb + (Tmax - Tb) * 0.2
                            x2 = Tb + (Tmax - Tb) * 0.4
                            x3 = Tb + (Tmax - Tb) * 0.6
                            x4 = Tb + (Tmax - Tb) * 0.8
                            x5 = Tb + (Tmax - Tb) * 0.9
                            p1 = CoolProp.PropsSI("L", "T", x1, "P", P, cprop.Name) * 1000
                            p2 = CoolProp.PropsSI("L", "T", x2, "P", P, cprop.Name) * 1000
                            p3 = CoolProp.PropsSI("L", "T", x3, "P", P, cprop.Name) * 1000
                            p4 = CoolProp.PropsSI("L", "T", x4, "P", P, cprop.Name) * 1000
                            p5 = CoolProp.PropsSI("L", "T", x5, "P", P, cprop.Name) * 1000
                            val = Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                        Catch ex As Exception
                            WriteWarningMessage(ex.Message.ToString & ". Estimating value using Ely-Hanley [Fluid: " & cprop.Name & "]")
                            val = MyBase.AUX_VAPTHERMCONDi(cprop, T, P)
                        End Try
                    End If
                Else
                    WriteWarningMessage("CoolProp Warning: unable to calculate Vapor Thermal Conductivity for " & sub1 & " at T = " & T & " K. Estimating value...")
                    val = MyBase.AUX_VAPTHERMCONDi(cprop, T, P)
                End If
            Else
                WriteWarningMessage("CoolProp Warning: compound " & sub1 & " not supported. Estimating Vapor Thermal Conductivity value...")
                val = MyBase.AUX_VAPTHERMCONDi(cprop, T, P)
            End If
            Return val
        End Function

        Public Overrides Function AUX_VAPVISCi(cprop As Interfaces.ICompoundConstantProperties, T As Double) As Double
            Dim sub1 = cprop.Name
            Dim Tmin, Tmax, val As Double
            Dim P = Me.CurrentMaterialStream.Phases(2).Properties.pressure.GetValueOrDefault
            If IsCompoundSupported(sub1) Then
                Tmin = CoolProp.Props1SI(cprop.Name, "TMIN")
                Tmax = CoolProp.Props1SI(cprop.Name, "TMAX")
                If T > Tmin And T < Tmax Then
                    Try
                        val = CoolProp.PropsSI("V", "T", T, "P", P, cprop.Name)
                    Catch ex As Exception
                        WriteWarningMessage(ex.Message.ToString & ". Estimating value [Fluid: " & cprop.Name & "]")
                        val = MyBase.AUX_VAPVISCi(cprop, T)
                    End Try
                Else
                    WriteWarningMessage("CoolProp Warning: unable to calculate Vapor Viscosity for " & sub1 & " at T = " & T & " K. Estimating value...")
                    val = MyBase.AUX_VAPVISCi(cprop, T)
                End If
            Else
                WriteWarningMessage("CoolProp Warning: compound " & sub1 & " not supported. Estimating Vapor Viscosity value...")
                val = MyBase.AUX_VAPVISCi(cprop, T)
            End If
            Return val
        End Function

        Public Function AUX_VAPVISCMIX(T As Double, P As Double, MM As Double) As Double
            Dim val As Double
            Dim i As Integer
            Dim Tmin, Tmax, Pmin, Pmax, Tb, Tc As Double
            Dim vk(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            i = 0
            Dim xv As Double = Me.CurrentMaterialStream.Phases(2).Properties.molarfraction.GetValueOrDefault
            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                If IsCompoundSupported(subst.Name) Then
                    Tmin = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMIN")
                    Tmax = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMAX")
                    Pmin = CoolProp.Props1SI(subst.ConstantProperties.Name, "PMIN")
                    Pmax = CoolProp.Props1SI(subst.ConstantProperties.Name, "PMAX")
                    If T > Tmin And T < Tmax And P > Pmin And P < Pmax Then
                        Tc = CoolProp.Props1SI(subst.ConstantProperties.Name, "TCRIT")
                        If T < Tc Then Tb = CoolProp.PropsSI("T", "P", P, "Q", 1, subst.ConstantProperties.Name) Else Tb = Tc
                        If T > Tb Then
                            Try
                                vk(i) = CoolProp.PropsSI("V", "T", T, "P", P, subst.ConstantProperties.Name)
                            Catch ex As Exception
                                WriteWarningMessage(ex.Message.ToString & ". Estimating value [Fluid: " & subst.ConstantProperties.Name & "]")
                                vk(i) = MyBase.AUX_VAPVISCi(subst.ConstantProperties, T)
                            End Try
                        Else
                            Try
                                WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Vapor Viscosity, compound " &
                                                       subst.ConstantProperties.Name & ". Extrapolating curve to obtain a value...")
                                Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                x1 = Tb + (Tmax - Tb) * 0.2
                                x2 = Tb + (Tmax - Tb) * 0.4
                                x3 = Tb + (Tmax - Tb) * 0.6
                                x4 = Tb + (Tmax - Tb) * 0.8
                                x5 = Tb + (Tmax - Tb) * 0.9
                                p1 = CoolProp.PropsSI("V", "T", x1, "P", P, subst.ConstantProperties.Name)
                                p2 = CoolProp.PropsSI("V", "T", x2, "P", P, subst.ConstantProperties.Name)
                                p3 = CoolProp.PropsSI("V", "T", x3, "P", P, subst.ConstantProperties.Name)
                                p4 = CoolProp.PropsSI("V", "T", x4, "P", P, subst.ConstantProperties.Name)
                                p5 = CoolProp.PropsSI("V", "T", x5, "P", P, subst.ConstantProperties.Name)
                                vk(i) = Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                            Catch ex As Exception
                                WriteWarningMessage(ex.Message.ToString & ". Estimating value [Fluid: " & subst.ConstantProperties.Name & "]")
                                vk(i) = MyBase.AUX_VAPVISCi(subst.ConstantProperties, T)
                            End Try
                        End If
                    Else
                        WriteWarningMessage("CoolProp Warning: unable to calculate Vapor Phase Viscosity for " &
                                                                              subst.ConstantProperties.Name & " at T = " & T & " K and P = " & P &
                                                                              " Pa. Estimating value...")
                        vk(i) = MyBase.AUX_VAPVISCi(subst.ConstantProperties, T)
                    End If
                Else
                    WriteWarningMessage("CoolProp Warning: compound " & subst.ConstantProperties.Name & " not supported. Estimating Vapor Viscosity...")
                    vk(i) = MyBase.AUX_VAPVISCi(subst.ConstantProperties, T)
                End If
                If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                vk(i) = subst.MoleFraction * vk(i)
                i = i + 1
            Next
            val = MathEx.Common.Sum(vk)
            Return val
        End Function

        Public Overrides Function AUX_LIQDENS(ByVal T As Double, Optional ByVal P As Double = 0.0, Optional ByVal Pvp As Double = 0.0, Optional ByVal phaseid As Integer = 3, Optional ByVal FORCE_EOS As Boolean = False) As Double

            Dim val As Double
            Dim i As Integer
            Dim vk(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            i = 0
            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(phaseid).Compounds.Values
                vk(i) = subst.MassFraction / Me.AUX_LIQDENSi(subst.ConstantProperties, T)
                If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                i = i + 1
            Next
            val = 1 / MathEx.Common.Sum(vk)

            Return val

        End Function

        Public Overrides Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double

            Dim val As Double
            Dim i As Integer
            Dim Tmin, Tmax, Pmin, Pmax, Tb, Tc As Double
            Dim vk(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            i = 0
            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                If IsCompoundSupported(subst.Name) Then
                    Tmin = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMIN")
                    Tmax = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMAX")
                    Pmin = CoolProp.Props1SI(subst.ConstantProperties.Name, "PMIN")
                    Pmax = CoolProp.Props1SI(subst.ConstantProperties.Name, "PMAX")
                    If T > Tmin And T < Tmax And P > Pmin And P < Pmax Then
                        Tc = CoolProp.Props1SI(subst.ConstantProperties.Name, "TCRIT")
                        If T < Tc Then Tb = CoolProp.PropsSI("T", "P", P, "Q", 1, subst.ConstantProperties.Name) Else Tb = Tc
                        If T > Tb Then
                            Try
                                vk(i) = CoolProp.PropsSI("D", "T", T, "P", P, subst.ConstantProperties.Name)
                            Catch ex As Exception
                                WriteWarningMessage(ex.Message.ToString & ". Estimating value [Fluid: " & subst.ConstantProperties.Name & "]")
                                vk(i) = 1 / (8.314 * val * T / P) * Me.AUX_MMM(Phase.Vapor) / 1000
                            End Try
                        Else
                            Try
                                WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Vapor Density, compound " &
                                          subst.ConstantProperties.Name & ". Extrapolating curve to obtain a value...")
                                Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                x1 = Tb + (Tmax - Tb) * 0.2
                                x2 = Tb + (Tmax - Tb) * 0.4
                                x3 = Tb + (Tmax - Tb) * 0.6
                                x4 = Tb + (Tmax - Tb) * 0.8
                                x5 = Tb + (Tmax - Tb) * 0.9
                                p1 = CoolProp.PropsSI("D", "T", x1, "P", P, subst.ConstantProperties.Name)
                                p2 = CoolProp.PropsSI("D", "T", x2, "P", P, subst.ConstantProperties.Name)
                                p3 = CoolProp.PropsSI("D", "T", x3, "P", P, subst.ConstantProperties.Name)
                                p4 = CoolProp.PropsSI("D", "T", x4, "P", P, subst.ConstantProperties.Name)
                                p5 = CoolProp.PropsSI("D", "T", x5, "P", P, subst.ConstantProperties.Name)
                                vk(i) = Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                            Catch ex As Exception
                                WriteWarningMessage(ex.Message.ToString & ". Estimating value [Fluid: " & subst.ConstantProperties.Name & "]")
                                vk(i) = 1 / (8.314 * val * T / P) * Me.AUX_MMM(Phase.Vapor) / 1000
                            End Try
                        End If
                    Else
                        WriteWarningMessage("CoolProp Warning: unable to calculate Vapor Phase Density for " &
                                                                              subst.ConstantProperties.Name & " at T = " & T & " K and P = " & P &
                                                                              " Pa. Estimating value...")
                        vk(i) = 1 / (8.314 * val * T / P) * Me.AUX_MMM(Phase.Vapor) / 1000
                    End If
                Else
                    WriteWarningMessage("CoolProp Warning: compound " & subst.ConstantProperties.Name & " not supported. Estimating Vapor Density...")
                    vk(i) = 1 / (8.314 * val * T / P) * Me.AUX_MMM(Phase.Vapor) / 1000
                End If
                If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                vk(i) = subst.MoleFraction * vk(i)
                i = i + 1
            Next
            val = MathEx.Common.Sum(vk)
            Return val

        End Function

        Public Overrides Sub DW_CalcCompPartialVolume(ByVal phase As Phase, ByVal T As Double, ByVal P As Double)

            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                subst.PartialVolume = 0.0#
            Next

        End Sub

        Public Overrides Function DW_CalcCp_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double

            Dim phaseID As Integer
            Select Case Phase1
                Case PropertyPackages.Phase.Mixture
                    phaseID = 0
                Case PropertyPackages.Phase.Vapor
                    phaseID = 2
                Case PropertyPackages.Phase.Liquid1
                    phaseID = 3
                Case PropertyPackages.Phase.Liquid2
                    phaseID = 4
                Case PropertyPackages.Phase.Liquid3
                    phaseID = 5
                Case PropertyPackages.Phase.Liquid
                    phaseID = 1
                Case PropertyPackages.Phase.Aqueous
                    phaseID = 6
                Case PropertyPackages.Phase.Solid
                    phaseID = 7
            End Select

            Dim val As Double
            Dim i As Integer
            Dim Tmin, Tmax, Pmin, Pmax, Tb, Tc As Double
            Dim vk(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Select Case Phase1
                Case Phase.Aqueous, Phase.Liquid, Phase.Liquid1, Phase.Liquid2, Phase.Liquid3
                    i = 0
                    For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(phaseID).Compounds.Values
                        If IsCompoundSupported(subst.Name) Then
                            Tmin = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMIN")
                            Tmax = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMAX")
                            Pmin = CoolProp.Props1SI(subst.ConstantProperties.Name, "PMIN")
                            Pmax = CoolProp.Props1SI(subst.ConstantProperties.Name, "PMAX")
                            Tc = CoolProp.Props1SI(subst.ConstantProperties.Name, "TCRIT")
                            If T > Tmin And T < Tmax And P > Pmin And P < Pmax And T <= Tc Then
                                Tb = CoolProp.PropsSI("T", "P", P, "Q", 1, subst.ConstantProperties.Name)
                                If T < Tb Then
                                    vk(i) = CoolProp.PropsSI("C", "T", T, "P", P, subst.ConstantProperties.Name) / 1000
                                Else
                                    WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Liquid Cp, compound " &
                                                           subst.ConstantProperties.Name & ". Extrapolating curve to obtain a value...")
                                    Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                    x1 = Tb * 0.98
                                    x2 = Tb * 0.94
                                    x3 = Tb * 0.92
                                    x4 = Tb * 0.86
                                    x5 = Tb * 0.8
                                    p1 = CoolProp.PropsSI("C", "T", x1, "P", P, subst.ConstantProperties.Name) / 1000
                                    p2 = CoolProp.PropsSI("C", "T", x2, "P", P, subst.ConstantProperties.Name) / 1000
                                    p3 = CoolProp.PropsSI("C", "T", x3, "P", P, subst.ConstantProperties.Name) / 1000
                                    p4 = CoolProp.PropsSI("C", "T", x4, "P", P, subst.ConstantProperties.Name) / 1000
                                    p5 = CoolProp.PropsSI("C", "T", x5, "P", P, subst.ConstantProperties.Name) / 1000
                                    vk(i) = Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                                End If
                            Else
                                WriteWarningMessage("CoolProp Warning: unable to calculate Cp for " & subst.ConstantProperties.Name & " at T = " & T & " K and P = " & P & " Pa.")
                                vk(i) = 0.0#
                            End If
                        Else
                            WriteWarningMessage("CoolProp Warning: compound " & subst.ConstantProperties.Name & " not supported.")
                            vk(i) = 0.0#
                        End If
                        If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                        vk(i) = subst.MassFraction * vk(i)
                        i = i + 1
                    Next
                Case Phase.Vapor
                    i = 0
                    For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(phaseID).Compounds.Values
                        If IsCompoundSupported(subst.Name) Then
                            Tmin = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMIN")
                            Tmax = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMAX")
                            Pmin = CoolProp.Props1SI(subst.ConstantProperties.Name, "PMIN")
                            Pmax = CoolProp.Props1SI(subst.ConstantProperties.Name, "PMAX")
                            If T > Tmin And T < Tmax And P > Pmin And P < Pmax Then
                                Tb = CoolProp.PropsSI("T", "P", P, "Q", 1, subst.ConstantProperties.Name)
                                If T > Tb Then
                                    vk(i) = CoolProp.PropsSI("C", "T", T, "P", P, subst.ConstantProperties.Name) / 1000
                                Else
                                    WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Vapor Cp, compound " &
                                                        subst.ConstantProperties.Name & ". Extrapolating curve to obtain a value...")
                                    Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                    x1 = Tb + (Tmax - Tb) * 0.2
                                    x2 = Tb + (Tmax - Tb) * 0.4
                                    x3 = Tb + (Tmax - Tb) * 0.6
                                    x4 = Tb + (Tmax - Tb) * 0.8
                                    x5 = Tb + (Tmax - Tb) * 0.9
                                    p1 = CoolProp.PropsSI("C", "T", x1, "P", P, subst.ConstantProperties.Name) / 1000
                                    p2 = CoolProp.PropsSI("C", "T", x2, "P", P, subst.ConstantProperties.Name) / 1000
                                    p3 = CoolProp.PropsSI("C", "T", x3, "P", P, subst.ConstantProperties.Name) / 1000
                                    p4 = CoolProp.PropsSI("C", "T", x4, "P", P, subst.ConstantProperties.Name) / 1000
                                    p5 = CoolProp.PropsSI("C", "T", x5, "P", P, subst.ConstantProperties.Name) / 1000
                                    vk(i) = Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                                End If
                            Else
                                WriteWarningMessage("CoolProp Warning: unable to calculate Cp for " & subst.ConstantProperties.Name & " at T = " & T & " K and P = " & P & " Pa.")
                                vk(i) = 0.0#
                            End If
                        Else
                            WriteWarningMessage("CoolProp Warning: compound " & subst.ConstantProperties.Name & " not supported.")
                            vk(i) = 0.0#
                        End If
                        If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                        vk(i) = subst.MassFraction * vk(i)
                        i = i + 1
                    Next
            End Select

            val = MathEx.Common.Sum(vk)

            Return val

        End Function

        Public Overrides Function DW_CalcCv_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double

            Dim phaseID As Integer
            Select Case Phase1
                Case PropertyPackages.Phase.Mixture
                    phaseID = 0
                Case PropertyPackages.Phase.Vapor
                    phaseID = 2
                Case PropertyPackages.Phase.Liquid1
                    phaseID = 3
                Case PropertyPackages.Phase.Liquid2
                    phaseID = 4
                Case PropertyPackages.Phase.Liquid3
                    phaseID = 5
                Case PropertyPackages.Phase.Liquid
                    phaseID = 1
                Case PropertyPackages.Phase.Aqueous
                    phaseID = 6
                Case PropertyPackages.Phase.Solid
                    phaseID = 7
            End Select

            Dim val As Double
            Dim i As Integer
            Dim Tmin, Tmax, Pmin, Pmax, Tb, Tc As Double
            Dim vk(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Select Case Phase1
                Case Phase.Aqueous, Phase.Liquid, Phase.Liquid1, Phase.Liquid2, Phase.Liquid3
                    i = 0
                    For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(phaseID).Compounds.Values
                        If IsCompoundSupported(subst.Name) Then
                            Tmin = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMIN")
                            Tmax = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMAX")
                            Pmin = CoolProp.Props1SI(subst.ConstantProperties.Name, "PMIN")
                            Pmax = CoolProp.Props1SI(subst.ConstantProperties.Name, "PMAX")
                            Tc = CoolProp.Props1SI(subst.ConstantProperties.Name, "TCRIT")
                            If T > Tmin And T < Tmax And P > Pmin And P < Pmax And T <= Tc Then
                                Tb = Me.AUX_TSATi(P, i)
                                If T < Tb Then
                                    vk(i) = CoolProp.PropsSI("O", "T", T, "P", P, subst.ConstantProperties.Name) / 1000
                                Else
                                    WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Liquid Cv, compound " &
                                                         subst.ConstantProperties.Name & ". Extrapolating curve to obtain a value...")
                                    Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                    x1 = Tb * 0.98
                                    x2 = Tb * 0.94
                                    x3 = Tb * 0.92
                                    x4 = Tb * 0.86
                                    x5 = Tb * 0.8
                                    p1 = CoolProp.PropsSI("O", "T", x1, "P", P, subst.ConstantProperties.Name) / 1000
                                    p2 = CoolProp.PropsSI("O", "T", x2, "P", P, subst.ConstantProperties.Name) / 1000
                                    p3 = CoolProp.PropsSI("O", "T", x3, "P", P, subst.ConstantProperties.Name) / 1000
                                    p4 = CoolProp.PropsSI("O", "T", x4, "P", P, subst.ConstantProperties.Name) / 1000
                                    p5 = CoolProp.PropsSI("O", "T", x5, "P", P, subst.ConstantProperties.Name) / 1000
                                    vk(i) = Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                                End If
                            Else
                                WriteWarningMessage("CoolProp Warning: unable to calculate Cv for " & subst.ConstantProperties.Name & " at T = " & T & " K and P = " & P & " Pa.")
                                vk(i) = 0.0#
                            End If
                        Else
                            WriteWarningMessage("CoolProp Warning: compound " & subst.ConstantProperties.Name & " not supported.")
                            vk(i) = 0.0#
                        End If
                        If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                        vk(i) = subst.MassFraction * vk(i)
                        i = i + 1
                    Next
                Case Phase.Vapor
                    i = 0
                    For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(phaseID).Compounds.Values
                        If IsCompoundSupported(subst.Name) Then
                            Tmin = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMIN")
                            Tmax = CoolProp.Props1SI(subst.ConstantProperties.Name, "TMAX")
                            Pmin = CoolProp.Props1SI(subst.ConstantProperties.Name, "PMIN")
                            Pmax = CoolProp.Props1SI(subst.ConstantProperties.Name, "PMAX")
                            If T > Tmin And T < Tmax And P > Pmin And P < Pmax Then
                                Tb = Me.AUX_TSATi(P, i)
                                If T > Tb Then
                                    vk(i) = CoolProp.PropsSI("O", "T", T, "P", P, subst.ConstantProperties.Name) / 1000
                                Else
                                    WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Vapor Cv, compound " &
                                                         subst.ConstantProperties.Name & ". Extrapolating curve to obtain a value...")
                                    Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                    x1 = Tb + (Tmax - Tb) * 0.2
                                    x2 = Tb + (Tmax - Tb) * 0.4
                                    x3 = Tb + (Tmax - Tb) * 0.6
                                    x4 = Tb + (Tmax - Tb) * 0.8
                                    x5 = Tb + (Tmax - Tb) * 0.9
                                    p1 = CoolProp.PropsSI("O", "T", x1, "P", P, subst.ConstantProperties.Name) / 1000
                                    p2 = CoolProp.PropsSI("O", "T", x2, "P", P, subst.ConstantProperties.Name) / 1000
                                    p3 = CoolProp.PropsSI("O", "T", x3, "P", P, subst.ConstantProperties.Name) / 1000
                                    p4 = CoolProp.PropsSI("O", "T", x4, "P", P, subst.ConstantProperties.Name) / 1000
                                    p5 = CoolProp.PropsSI("O", "T", x5, "P", P, subst.ConstantProperties.Name) / 1000
                                    vk(i) = Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                                End If
                            Else
                                WriteWarningMessage("CoolProp Warning: unable to calculate Cv for " & subst.ConstantProperties.Name & " at T = " & T & " K and P = " & P & " Pa.")
                                vk(i) = 0.0#
                            End If
                        Else
                            WriteWarningMessage("CoolProp Warning: compound " & subst.ConstantProperties.Name & " not supported.")
                            vk(i) = 0.0#
                        End If
                        If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                        vk(i) = subst.MassFraction * vk(i)
                        i = i + 1
                    Next
            End Select

            val = MathEx.Common.Sum(vk)

            Return val

        End Function

        Public Overrides Function DW_CalcEnthalpy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Calculator.CheckParallelPInvoke()

            Dim val As Double
            Dim i As Integer
            Dim vk(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim xv As Double = Me.CurrentMaterialStream.Phases(2).Properties.molarfraction.GetValueOrDefault
            Dim vn As String() = Me.RET_VNAMES
            Dim Vxw As Double() = AUX_CONVERT_MOL_TO_MASS(Vx)
            Dim n As Integer = Vx.Length - 1
            Dim Tmin, Tmax, Pmin, Pmax, Tb As Double
            Select Case st
                Case State.Liquid
                    For i = 0 To n
                        If IsCompoundSupported(vn(i)) Then
                            Tmin = CoolProp.Props1SI(vn(i), "TMIN")
                            Tmax = CoolProp.Props1SI(vn(i), "TMAX")
                            Pmin = CoolProp.Props1SI(vn(i), "PMIN")
                            Pmax = CoolProp.Props1SI(vn(i), "PMAX")
                            If P > Pmin And P < Pmax Then
                                Tb = Me.AUX_TSATi(P, i)
                                If T < Tb And T > Tmin Then
                                    vk(i) = CoolProp.PropsSI("H", "T", T, "P", P, vn(i)) / 1000
                                Else
                                    WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Liquid Enthalpy, compound " &
                                                         vn(i) & ". Extrapolating curve to obtain a value...")
                                    Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                    x1 = Tb * 0.98
                                    x2 = Tb * 0.94
                                    x3 = Tb * 0.92
                                    x4 = Tb * 0.86
                                    x5 = Tb * 0.8
                                    p1 = CoolProp.PropsSI("H", "T", x1, "P", P, vn(i)) / 1000
                                    p2 = CoolProp.PropsSI("H", "T", x2, "P", P, vn(i)) / 1000
                                    p3 = CoolProp.PropsSI("H", "T", x3, "P", P, vn(i)) / 1000
                                    p4 = CoolProp.PropsSI("H", "T", x4, "P", P, vn(i)) / 1000
                                    p5 = CoolProp.PropsSI("H", "T", x5, "P", P, vn(i)) / 1000
                                    vk(i) = Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                                End If
                            Else
                                WriteWarningMessage("CoolProp Warning: unable to calculate Enthalpy for " & vn(i) & " at T = " & T & " K and P = " & P & " Pa.")
                                vk(i) = 0.0#
                            End If
                        Else
                            WriteWarningMessage("CoolProp Warning: compound " & vn(i) & " not supported.")
                            vk(i) = 0.0#
                        End If
                        If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                        vk(i) = Vxw(i) * vk(i)
                    Next
                Case State.Vapor
                    For i = 0 To n
                        If IsCompoundSupported(vn(i)) Then
                            Tmin = CoolProp.Props1SI(vn(i), "TMIN")
                            Tmax = CoolProp.Props1SI(vn(i), "TMAX")
                            Pmin = CoolProp.Props1SI(vn(i), "PMIN")
                            Pmax = CoolProp.Props1SI(vn(i), "PMAX")
                            If P > Pmin And P < Pmax Then
                                Tb = Me.AUX_TSATi(P, i)
                                If T > Tb Then
                                    vk(i) = CoolProp.PropsSI("H", "T", T, "P", P, vn(i)) / 1000
                                Else
                                    WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Vapor Enthalpy, compound " &
                                                         vn(i) & ". Extrapolating curve to obtain a value...")
                                    Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                    x1 = Tb + (Tmax - Tb) * 0.2
                                    x2 = Tb + (Tmax - Tb) * 0.4
                                    x3 = Tb + (Tmax - Tb) * 0.6
                                    x4 = Tb + (Tmax - Tb) * 0.8
                                    x5 = Tb + (Tmax - Tb) * 0.9
                                    p1 = CoolProp.PropsSI("H", "T", x1, "P", P, vn(i)) / 1000
                                    p2 = CoolProp.PropsSI("H", "T", x2, "P", P, vn(i)) / 1000
                                    p3 = CoolProp.PropsSI("H", "T", x3, "P", P, vn(i)) / 1000
                                    p4 = CoolProp.PropsSI("H", "T", x4, "P", P, vn(i)) / 1000
                                    p5 = CoolProp.PropsSI("H", "T", x5, "P", P, vn(i)) / 1000
                                    vk(i) = Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                                End If
                            Else
                                WriteWarningMessage("CoolProp Warning: unable to calculate Enthalpy for " & vn(i) & " at T = " & T & " K and P = " & P & " Pa.")
                                vk(i) = 0.0#
                            End If
                        Else
                            WriteWarningMessage("CoolProp Warning: compound " & vn(i) & " not supported.")
                            vk(i) = 0.0#
                        End If
                        If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                        vk(i) = Vxw(i) * vk(i)
                    Next
            End Select

            val = MathEx.Common.Sum(vk)

            Return val

        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Return DW_CalcEnthalpy(Vx, T, P, st)

        End Function

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Calculator.CheckParallelPInvoke()

            Dim val As Double
            Dim i As Integer
            Dim vk(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim xv As Double = Me.CurrentMaterialStream.Phases(2).Properties.molarfraction.GetValueOrDefault
            Dim vn As String() = Me.RET_VNAMES
            Dim Vxw As Double() = AUX_CONVERT_MOL_TO_MASS(Vx)
            Dim n As Integer = Vx.Length - 1
            Dim Tmin, Tmax, Pmin, Pmax, Tb As Double
            Select Case st
                Case State.Liquid
                    For i = 0 To n
                        If IsCompoundSupported(vn(i)) Then
                            Tmin = CoolProp.Props1SI(vn(i), "TMIN")
                            Tmax = CoolProp.Props1SI(vn(i), "TMAX")
                            Pmin = CoolProp.Props1SI(vn(i), "PMIN")
                            Pmax = CoolProp.Props1SI(vn(i), "PMAX")
                            If P > Pmin And P < Pmax Then
                                Tb = Me.AUX_TSATi(P, i)
                                If T < Tb And T > Tmin Then
                                    vk(i) = CoolProp.PropsSI("S", "T", T, "P", P, vn(i)) / 1000
                                Else
                                    WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Liquid Entropy, compound " &
                                                         vn(i) & ". Extrapolating curve to obtain a value...")
                                    Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                    x1 = Tb * 0.98
                                    x2 = Tb * 0.94
                                    x3 = Tb * 0.92
                                    x4 = Tb * 0.86
                                    x5 = Tb * 0.8
                                    p1 = CoolProp.PropsSI("S", "T", x1, "P", P, vn(i)) / 1000
                                    p2 = CoolProp.PropsSI("S", "T", x2, "P", P, vn(i)) / 1000
                                    p3 = CoolProp.PropsSI("S", "T", x3, "P", P, vn(i)) / 1000
                                    p4 = CoolProp.PropsSI("S", "T", x4, "P", P, vn(i)) / 1000
                                    p5 = CoolProp.PropsSI("S", "T", x5, "P", P, vn(i)) / 1000
                                    vk(i) = Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                                End If
                            Else
                                WriteWarningMessage("CoolProp Warning: unable to calculate Enthalpy for " & vn(i) & " at T = " & T & " K and P = " & P & " Pa.")
                                vk(i) = 0.0#
                            End If
                        Else
                            WriteWarningMessage("CoolProp Warning: compound " & vn(i) & " not supported.")
                            vk(i) = 0.0#
                        End If
                        If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                        vk(i) = Vxw(i) * vk(i)
                    Next
                Case State.Vapor
                    For i = 0 To n
                        If IsCompoundSupported(vn(i)) Then
                            Tmin = CoolProp.Props1SI(vn(i), "TMIN")
                            Tmax = CoolProp.Props1SI(vn(i), "TMAX")
                            Pmin = CoolProp.Props1SI(vn(i), "PMIN")
                            Pmax = CoolProp.Props1SI(vn(i), "PMAX")
                            If P > Pmin And P < Pmax Then
                                Tb = Me.AUX_TSATi(P, i)
                                If T > Tb Then
                                    vk(i) = CoolProp.PropsSI("S", "T", T, "P", P, vn(i)) / 1000
                                Else
                                    WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Vapor Entropy, compound " &
                                                         vn(i) & ". Extrapolating curve to obtain a value...")
                                    Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                    x1 = Tb + (Tmax - Tb) * 0.2
                                    x2 = Tb + (Tmax - Tb) * 0.4
                                    x3 = Tb + (Tmax - Tb) * 0.6
                                    x4 = Tb + (Tmax - Tb) * 0.8
                                    x5 = Tb + (Tmax - Tb) * 0.9
                                    p1 = CoolProp.PropsSI("S", "T", x1, "P", P, vn(i)) / 1000
                                    p2 = CoolProp.PropsSI("S", "T", x2, "P", P, vn(i)) / 1000
                                    p3 = CoolProp.PropsSI("S", "T", x3, "P", P, vn(i)) / 1000
                                    p4 = CoolProp.PropsSI("S", "T", x4, "P", P, vn(i)) / 1000
                                    p5 = CoolProp.PropsSI("S", "T", x5, "P", P, vn(i)) / 1000
                                    vk(i) = Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                                End If
                            Else
                                WriteWarningMessage("CoolProp Warning: unable to calculate Enthalpy for " & vn(i) & " at T = " & T & " K and P = " & P & " Pa.")
                                vk(i) = 0.0#
                            End If
                        Else
                            WriteWarningMessage("CoolProp Warning: compound " & vn(i) & " not supported.")
                            vk(i) = 0.0#
                        End If
                        If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                        vk(i) = Vxw(i) * vk(i)
                    Next
            End Select

            val = MathEx.Common.Sum(vk)

            Return val

        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Return DW_CalcEntropy(Vx, T, P, st)

        End Function

        Public Overrides Function DW_CalcFugCoeff(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

            Calculator.WriteToConsole(Me.ComponentName & " fugacity coefficient calculation for phase '" & st.ToString & "' requested at T = " & T & " K and P = " & P & " Pa.", 2)
            Calculator.WriteToConsole("Compounds: " & Me.RET_VNAMES.ToArrayString, 2)
            Calculator.WriteToConsole("Mole fractions: " & Vx.ToArrayString(), 2)

            Dim n As Integer = UBound(Vx)
            Dim i As Integer
            Dim fugcoeff(n) As Double

            If st = State.Liquid Then
                Dim Tc As Object = Me.RET_VTC()
                For i = 0 To n
                    If T / Tc(i) >= 1 Then
                        fugcoeff(i) = AUX_KHenry(Me.RET_VNAMES(i), T) / P
                    Else
                        fugcoeff(i) = Me.AUX_PVAPi(i, T) / P
                    End If
                Next
            Else
                For i = 0 To n
                    fugcoeff(i) = 1
                Next
            End If

            Calculator.WriteToConsole("Result: " & fugcoeff.ToArrayString(), 2)

            Return fugcoeff

        End Function

        Public Overrides Function DW_CalcK_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double

            Return 0.0#

        End Function

        Public Overrides Function DW_CalcMassaEspecifica_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double, Optional ByVal Pvp As Double = 0.0) As Double

            Return 0.0#

        End Function

        Public Overrides Function DW_CalcMM_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double

            Return Me.AUX_MMM(Phase1)

        End Function

        Public Overrides Sub DW_CalcPhaseProps(ByVal Phase As Phase)

            Calculator.CheckParallelPInvoke()

            Dim result As Double
            Dim dwpl As Phase

            Dim T, P As Double
            Dim phasemolarfrac As Double = Nothing
            Dim overallmolarflow As Double = Nothing

            Dim phaseID As Integer
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Select Case Phase
                Case PropertyPackages.Phase.Mixture
                    phaseID = 0
                    dwpl = PropertyPackages.Phase.Mixture
                Case PropertyPackages.Phase.Vapor
                    phaseID = 2
                    dwpl = PropertyPackages.Phase.Vapor
                Case PropertyPackages.Phase.Liquid1
                    phaseID = 3
                    dwpl = PropertyPackages.Phase.Liquid1
                Case PropertyPackages.Phase.Liquid2
                    phaseID = 4
                    dwpl = PropertyPackages.Phase.Liquid2
                Case PropertyPackages.Phase.Liquid3
                    phaseID = 5
                    dwpl = PropertyPackages.Phase.Liquid3
                Case PropertyPackages.Phase.Liquid
                    phaseID = 1
                    dwpl = PropertyPackages.Phase.Liquid
                Case PropertyPackages.Phase.Aqueous
                    phaseID = 6
                    dwpl = PropertyPackages.Phase.Aqueous
                Case PropertyPackages.Phase.Solid
                    phaseID = 7
                    dwpl = PropertyPackages.Phase.Solid
            End Select

            If phaseID > 0 Then

                overallmolarflow = Me.CurrentMaterialStream.Phases(0).Properties.molarflow.GetValueOrDefault
                phasemolarfrac = Me.CurrentMaterialStream.Phases(phaseID).Properties.molarfraction.GetValueOrDefault
                result = overallmolarflow * phasemolarfrac
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molarflow = result
                result = result * Me.AUX_MMM(Phase) / 1000
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow = result
                result = phasemolarfrac * overallmolarflow * Me.AUX_MMM(Phase) / 1000 / Me.CurrentMaterialStream.Phases(0).Properties.massflow.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massfraction = result
                Me.DW_CalcCompVolFlow(phaseID)
                Me.DW_CalcCompFugCoeff(Phase)
            End If

            If phaseID = 3 Or phaseID = 4 Or phaseID = 5 Or phaseID = 6 Then


                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = AUX_LIQDENS(T, P, 0.0#, phaseID)

                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result

                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result

                result = P / (Me.AUX_LIQDENS(T, P, 0, phaseID) * 8.314 * T) / 1000 * AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result

                result = Me.DW_CalcCp_ISOL(Phase, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result

                result = Me.DW_CalcCv_ISOL(Phase, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result

                result = Me.AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result

                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid) * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result

                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid) * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result

                result = Me.AUX_CONDTL(T)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result

                result = Me.AUX_LIQVISCm(T)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result

                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

            ElseIf phaseID = 2 Then

                result = Me.AUX_VAPDENS(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result

                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result

                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result

                result = P / (Me.AUX_VAPDENS(T, P) * 8.314 * T) / 1000 * AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result

                result = Me.DW_CalcCp_ISOL(Phase, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result

                result = Me.DW_CalcCv_ISOL(Phase, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result

                result = Me.AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result

                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor) * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result

                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor) * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result

                result = Me.AUX_CONDTG(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result

                result = Me.AUX_VAPVISCMIX(T, P, Me.AUX_MMM(Phase))
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault

            ElseIf phaseID = 1 Then

                DW_CalcLiqMixtureProps()


            Else

                DW_CalcOverallProps()

            End If


            If phaseID > 0 Then
                result = overallmolarflow * phasemolarfrac * Me.AUX_MMM(Phase) / 1000 / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = result
            End If


        End Sub

        Public Overrides Sub DW_CalcProp(ByVal [property] As String, ByVal phase As Phase)

            Calculator.CheckParallelPInvoke()

            Dim result As Double = 0.0#
            Dim resultObj As Object = Nothing
            Dim phaseID As Integer = -1
            Dim state As String = ""
            Dim fstate As State = PropertyPackages.State.Solid

            Dim T, P As Double
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Select Case phase
                Case Phase.Vapor
                    state = "V"
                    fstate = PropertyPackages.State.Vapor
                Case Phase.Liquid, Phase.Liquid1, Phase.Liquid2, Phase.Liquid3, Phase.Aqueous
                    state = "L"
                    fstate = PropertyPackages.State.Liquid
                Case Phase.Solid
                    state = "S"
                    fstate = PropertyPackages.State.Solid
            End Select

            Select Case phase
                Case PropertyPackages.Phase.Mixture
                    phaseID = 0
                Case PropertyPackages.Phase.Vapor
                    phaseID = 2
                Case PropertyPackages.Phase.Liquid1
                    phaseID = 3
                Case PropertyPackages.Phase.Liquid2
                    phaseID = 4
                Case PropertyPackages.Phase.Liquid3
                    phaseID = 5
                Case PropertyPackages.Phase.Liquid
                    phaseID = 1
                Case PropertyPackages.Phase.Aqueous
                    phaseID = 6
                Case PropertyPackages.Phase.Solid
                    phaseID = 7
            End Select

            Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = Me.AUX_MMM(phase)

            Select Case [property].ToLower
                Case "compressibilityfactor"
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                Case "heatcapacity", "heatcapacitycp"
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                Case "heatcapacitycv"
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                Case "enthalpy", "enthalpynf"
                    result = Me.DW_CalcEnthalpy(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = result * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                Case "entropy", "entropynf"
                    result = Me.DW_CalcEntropy(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                    result = result * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                Case "excessenthalpy"
                    result = Me.DW_CalcEnthalpyDeparture(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEnthalpy = result
                Case "excessentropy"
                    result = Me.DW_CalcEntropyDeparture(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEntropy = result
                Case "enthalpyf"
                    Dim entF As Double = 0.0#
                    result = Me.DW_CalcEnthalpy(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = result + entF
                    result = result * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = result
                Case "entropyf"
                    Dim entF As Double = 0.0#
                    result = Me.DW_CalcEntropy(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF = result + entF
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropyF = result
                Case "viscosity"
                    If state = "L" Then
                        result = Me.AUX_LIQVISCm(T)
                    Else
                        result = Me.AUX_VAPVISCMIX(T, P, Me.AUX_MMM(phase))
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Case "thermalconductivity"
                    If state = "L" Then
                        result = Me.AUX_CONDTL(T)
                    Else
                        result = Me.AUX_CONDTG(T, P)
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                Case "fugacity", "fugacitycoefficient", "logfugacitycoefficient", "activity", "activitycoefficient"
                    Me.DW_CalcCompFugCoeff(phase)
                Case "volume", "density"
                    If state = "L" Then
                        result = Me.AUX_LIQDENS(T, P, 0.0#, phaseID, False)
                    Else
                        result = Me.AUX_VAPDENS(T, P)
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                Case "surfacetension"
                    Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = Me.AUX_SURFTM(T)
                Case Else
                    Dim ex As Exception = New CapeOpen.CapeThrmPropertyNotAvailableException
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "CalcSinglePhaseProp/CalcTwoPhaseProp/CalcProp", ex.GetHashCode)
            End Select

        End Sub

        Public Overrides Function DW_CalcPVAP_ISOL(ByVal T As Double) As Double

            Return 0.0#

        End Function

        Public Overrides Function DW_CalcTensaoSuperficial_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double

            Return Me.AUX_SURFTM(T)

        End Function

        Public Overrides Sub DW_CalcTwoPhaseProps(ByVal Phase1 As Phase, ByVal Phase2 As Phase)

            Dim T As Double

            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = Me.AUX_SURFTM(T)

        End Sub

        Public Overrides Function DW_CalcViscosidadeDinamica_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double

            If Phase1 = Phase.Liquid Then
                Return Me.AUX_LIQVISCm(T)
            ElseIf Phase1 = Phase.Vapor Then
                Return Me.AUX_VAPVISCm(T, Me.AUX_VAPDENS(T, P), Me.AUX_MMM(Phase.Vapor))
            End If

        End Function

        Public Overrides Function SupportsComponent(ByVal comp As Interfaces.ICompoundConstantProperties) As Boolean

            CheckIfCompoundIsSupported(comp.Name)

            Return True

        End Function

        Public Overrides Function DW_CalcEnergyFlowMistura_ISOL(T As Double, P As Double) As Double

        End Function

#End Region

#Region "    Auxiliary Functions"

        Sub CheckIfCompoundIsSupported(compname As String)

            Dim comps() As String = CoolProp.get_global_param_string("FluidsList").Split(",")

            If Not comps.Contains(compname) Then
                Throw New ArgumentOutOfRangeException(compname, "Error: compound '" & compname & "' is not supported by this version of CoolProp.")
            End If

        End Sub

        Function IsCompoundSupported(compname As String) As Boolean

            Dim comps() As String = CoolProp.get_global_param_string("FluidsList").Split(",")

            If Not comps.Contains(compname) Then Return False Else Return True

        End Function

#End Region

    End Class

End Namespace
