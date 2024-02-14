'    CoolProp Property Package
'    Copyright 2014-2024 Daniel Wagner O. de Medeiros
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
Imports DWSIM.Interfaces.Enums
Imports System.IO
Imports System.Reflection

Namespace PropertyPackages


    <System.Runtime.InteropServices.Guid(CoolPropPropertyPackage.ClassId)>
    <System.Serializable()> Public Class CoolPropPropertyPackage

        Inherits PropertyPackages.PropertyPackage

        Public Shadows Const ClassId As String = "1F5B0263-E936-40d5-BA5B-FFAB11595E43"

        Public CompoundAliases As New Dictionary(Of String, List(Of String))

        <NonSerialized> Private _IObj As InspectorItem

        Public Overrides ReadOnly Property Popular As Boolean = True

        Public Sub New(ByVal comode As Boolean)
            MyBase.New(comode)
            GetListOfSupportedCompounds()
        End Sub

        Public Sub New()

            MyBase.New()

            GetListOfSupportedCompounds()

            Me.IsConfigurable = True
            Me._packagetype = PropertyPackages.PackageType.Miscelaneous

            With PropertyMethodsInfo
                .Vapor_Fugacity = "Ideal"
                .Vapor_Thermal_Conductivity = "CoolProp"
                .Vapor_Viscosity = "CoolProp"
                .Vapor_Enthalpy_Entropy_CpCv = "CoolProp"
                .Vapor_Density = "CoolProp"
                .Liquid_Fugacity = "Vapor Pressure"
                .Liquid_Enthalpy_Entropy_CpCv = "CoolProp"
                .Liquid_ThermalConductivity = "CoolProp"
                .Liquid_Viscosity = "CoolProp"
            End With

        End Sub

        Sub GetListOfSupportedCompounds()

            Dim comps As List(Of String)
            'comps= CoolProp.get_global_param_string("FluidsList").Split(",").ToList()

            Using filestr As Stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("DWSIM.Thermodynamics.CoolPropFluids.txt")
                Using t As New StreamReader(filestr)
                    comps = t.ReadToEnd().Split(vbLf).ToList()
                End Using
            End Using

            comps = comps.Select(Function(a) a.Trim()).ToList()

            'IO.File.WriteAllLines("CoolPropFluids.txt", comps.ToArray())

            CompoundAliases.Clear()
            SupportedComponents.Clear()

            Dim data As String()

            Using filestr As Stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("DWSIM.Thermodynamics.CoolPropFluidAliases.txt")
                Using t As New StreamReader(filestr)
                    data = t.ReadToEnd().Split(vbLf)
                End Using
            End Using

            Dim aliases As New List(Of String), cas As String

            Dim i = 0
            For Each c In comps
                Dim sdata = data(i).Split(vbTab)
                aliases = sdata(1).Split(",").ToList()
                aliases = aliases.Select(Function(a) a.Trim()).Where(Function(a2) a2 <> "" And a2 <> " ").ToList()
                aliases.Add(c)
                SupportedComponents.AddRange(aliases)
                cas = sdata(0)
                CompoundAliases.Add(cas, aliases.ToList)
                i += 1
            Next

            'For Each c In comps
            '    aliases = CoolProp.get_fluid_param_string(c, "aliases").Split(",").ToList()
            '    aliases = aliases.Select(Function(a) a.Trim()).Where(Function(a2) a2 <> "" And a2 <> " ").ToList()
            '    aliases.Add(c)
            '    SupportedComponents.AddRange(aliases)
            '    cas = CoolProp.get_fluid_param_string(c, "CAS")
            '    CompoundAliases.Add(cas, aliases.ToList)
            'Next

            'For Each item In CompoundAliases
            '    Dim al = ""
            '    For Each ali In item.Value
            '        al += ali + ","
            '    Next
            '    al.TrimEnd(",")
            '    IO.File.AppendAllText("CoolPropFluidAliases.txt", item.Key & vbTab & al & vbCrLf)
            'Next

        End Sub

        Function GetCoolPropName(name As String) As String
            Dim casid As String = CurrentMaterialStream.Phases(0).Compounds(name).ConstantProperties.CAS_Number
            Return CompoundAliases(casid)(0)
        End Function

        Private Sub SetCPDebugLevel()

            If GlobalSettings.Settings.InspectorEnabled Then
                CoolProp.set_debug_level(100000)
            Else
                CoolProp.set_debug_level(0)
            End If

        End Sub

#Region "    DWSIM Functions"

        Private Sub WriteWarningMessage(message As String)
            _IObj?.Paragraphs.Add(message)
            Select Case Settings.DebugLevel
                Case 0
                    'do nothing
                Case Else
                    Console.WriteLine(message)
            End Select
        End Sub

        Public Overrides Function AUX_CPi(sub1 As String, T As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Ideal Gas Heat Capacity - {0})", sub1)
            Inspector.Host.CheckAndAdd(IObj, "", "AUX_CPi", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))

            Dim Tmin, Tmax, Tc, val As Double
            If IsCompoundSupported(1.0, sub1) Then
                Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                Tc = CoolProp.Props1SI(GetCoolPropName(sub1), "TCRIT")
                If T > Tmin And T <= Tmax Then
                    Try
                        If T <= Tc Then
                            val = CoolProp.PropsSI("CP0MASS", "T", T, "Q", 1, GetCoolPropName(sub1)) / 1000
                        Else
                            val = CoolProp.PropsSI("CP0MASS", "T", T, "P", 101325, GetCoolPropName(sub1)) / 1000
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

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Ideal Gas Heat Capacity: {0} kJ/[kg.K]", val))
            IObj?.Close()

            Return val
        End Function

        Public Overrides Function AUX_PVAPi(index As Integer, T As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Vapor Pressure - {0})", index)
            Inspector.Host.CheckAndAdd(IObj, "", "AUX_PVAPi", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))

            Dim sub1 As String = RET_VNAMES()(index)
            Dim Tmin, Tmax, Tc, val As Double
            If IsCompoundSupported(1.0, sub1) Then
                Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                Tc = CoolProp.Props1SI(GetCoolPropName(sub1), "TCRIT")
                If T > Tmin And T <= Tmax And T <= Tc Then
                    Try
                        val = CoolProp.PropsSI("P", "T", T, "Q", 0, GetCoolPropName(sub1))
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

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Vapor Pressure: {0} Pa", val))
            IObj?.Close()

            Return val
        End Function

        Public Overrides Function AUX_PVAPi(sub1 As String, T As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Vapor Pressure - {0})", sub1)
            Inspector.Host.CheckAndAdd(IObj, "", "AUX_PVAPi", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))

            Dim Tmin, Tmax, Tc, val As Double
            If IsCompoundSupported(1.0, sub1) Then
                Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                Tc = CoolProp.Props1SI(GetCoolPropName(sub1), "TCRIT")
                If T > Tmin And T <= Tmax And T <= Tc Then
                    Try
                        val = CoolProp.PropsSI("P", "T", T, "Q", 0, GetCoolPropName(sub1))
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

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Vapor Pressure: {0} Pa", val))
            IObj?.Close()

            Return val
        End Function

        Public Overrides Function AUX_TSATi(PVAP As Double, index As Integer) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Saturation Temperature - {0})", index)
            Inspector.Host.CheckAndAdd(IObj, "", "AUX_TSATi", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Vapor Pressure: {0} Pa", PVAP))

            Dim sub1 As String = RET_VNAMES()(index)
            Dim Pmin, Pmax, val As Double
            If IsCompoundSupported(1.0, sub1) Then
                Pmin = CoolProp.Props1SI(GetCoolPropName(sub1), "PMIN")
                Pmax = CoolProp.Props1SI(GetCoolPropName(sub1), "PMAX")
                If PVAP > Pmin And PVAP < Pmax Then
                    Try
                        val = CoolProp.PropsSI("T", "P", PVAP, "Q", 0, GetCoolPropName(sub1))
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

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Saturation Temperature: {0} K", val))
            IObj?.Close()

            Return val
        End Function

        Public Overrides Function AUX_TSATi(PVAP As Double, subst As String) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Saturation Temperature - {0})", subst)
            Inspector.Host.CheckAndAdd(IObj, "", "AUX_TSATi", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Vapor Pressure: {0} Pa", PVAP))

            Dim Pmin, Pmax, val As Double
            If IsCompoundSupported(1.0, subst) Then
                Pmin = CoolProp.Props1SI(GetCoolPropName(subst), "PMIN")
                Pmax = CoolProp.Props1SI(GetCoolPropName(subst), "PMAX")
                If PVAP > Pmin And PVAP < Pmax Then
                    Try
                        val = CoolProp.PropsSI("T", "P", PVAP, "Q", 0, GetCoolPropName(subst))
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

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Saturation Temperature: {0} K", val))
            IObj?.Close()

            Return val
        End Function

        Public Overrides Function AUX_LIQDENSi(cprop As Interfaces.ICompoundConstantProperties, T As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Liquid Density - {0})", cprop.Name)
            Inspector.Host.CheckAndAdd(IObj, "", "AUX_LIQDENSi", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))

            Dim sub1 = cprop.Name
            Dim Tmin, Tmax, Tc, val As Double
            If IsCompoundSupported(1.0, sub1) Then
                Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                Tc = CoolProp.Props1SI(sub1, "TCRIT")
                If T > Tmin And T <= Tmax And T <= Tc Then
                    Try
                        val = CoolProp.PropsSI("D", "T", T, "Q", 0, GetCoolPropName(sub1))
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

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Density: {0} kg/m3", val))
            IObj?.Close()

            Return val
        End Function

        Public Overrides Function AUX_LIQ_Cpi(cprop As Interfaces.ICompoundConstantProperties, T As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Liquid Heat Capacity - {0})", cprop.Name)
            Inspector.Host.CheckAndAdd(IObj, "", "AUX_LIQ_Cpi", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))

            Dim sub1 = cprop.Name
            Dim Tmin, Tmax, Tc, val As Double
            If IsCompoundSupported(1.0, sub1) Then
                Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                Tc = CoolProp.Props1SI(GetCoolPropName(sub1), "TCRIT")
                If T > Tmin And T <= Tmax And T <= Tc Then
                    Try
                        val = CoolProp.PropsSI("C", "T", T, "Q", 0, GetCoolPropName(sub1)) / 1000
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

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Liquid Cp: {0} kJ/[kg.K]", val))
            IObj?.Close()

            Return val
        End Function

        Public Overrides Function AUX_CONDTG(T As Double, P As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Vapor Thermal Conductivity)")
            Inspector.Host.CheckAndAdd(IObj, "", "AUX_CONDTG", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))

            Dim val As Double
            Dim i As Integer
            Dim Tmin, Tmax, Pmin, Pmax, Tb, Tc As Double
            Dim vk(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            i = 0
            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                If subst.MassFraction.GetValueOrDefault() > 0.0 Then
                    If IsCompoundSupported(1.0, subst.Name) Then
                        Dim sub1 As String = subst.Name
                        Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                        Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                        Pmin = CoolProp.Props1SI(GetCoolPropName(sub1), "PMIN")
                        Pmax = CoolProp.Props1SI(GetCoolPropName(sub1), "PMAX")
                        Tc = CoolProp.Props1SI(GetCoolPropName(sub1), "TCRIT")
                        If T > Tmin And T < Tmax And P > Pmin And P < Pmax Then
                            If T < Tc Then Tb = CoolProp.PropsSI("T", "P", P, "Q", 1, GetCoolPropName(sub1)) Else Tb = Tc
                            If T > Tb And Abs(T - Tb) > 0.01 Then
                                Try
                                    vk(i) = CoolProp.PropsSI("L", "T", T, "P", P, GetCoolPropName(sub1))
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
                                    p1 = CoolProp.PropsSI("L", "T", x1, "P", P, GetCoolPropName(sub1))
                                    p2 = CoolProp.PropsSI("L", "T", x2, "P", P, GetCoolPropName(sub1))
                                    p3 = CoolProp.PropsSI("L", "T", x3, "P", P, GetCoolPropName(sub1))
                                    p4 = CoolProp.PropsSI("L", "T", x4, "P", P, GetCoolPropName(sub1))
                                    p5 = CoolProp.PropsSI("L", "T", x5, "P", P, GetCoolPropName(sub1))
                                    vk(i) = MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
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
                End If
                vk(i) = subst.MassFraction.GetValueOrDefault * vk(i)
                i = i + 1
            Next
            val = MathEx.Common.Sum(vk)

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Thermal Conductivity: {0} W/[m.K]", val))
            IObj?.Close()

            Return val
        End Function

        Public Overrides Function AUX_CONDTL(T As Double, Optional phaseid As Integer = 3) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Liquid Thermal Conductivity)")
            Inspector.Host.CheckAndAdd(IObj, "", "AUX_CONDTL", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))

            Dim val As Double
            Dim i As Integer
            Dim Tmin, Tmax, Tb, Tc As Double
            Dim vk(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            i = 0
            Dim P As Double = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault
            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(phaseid).Compounds.Values
                If subst.MassFraction.GetValueOrDefault() > 0.0 Then
                    If IsCompoundSupported(1.0, subst.Name) Then
                        Dim sub1 As String = subst.Name
                        Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                        Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                        Tc = CoolProp.Props1SI(GetCoolPropName(sub1), "TCRIT")
                        If T > Tmin And T <= Tmax And T <= Tc Then
                            Try
                                Tb = Me.AUX_TSATi(P, i)
                                If T < Tb And Abs(T - Tb) > 0.01 Then
                                    vk(i) = CoolProp.PropsSI("L", "T", T, "Q", 0, GetCoolPropName(sub1))
                                Else
                                    WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Liquid Thermal Conductivity, compound " &
                          subst.ConstantProperties.Name & ". Extrapolating curve to obtain a value...")
                                    Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                    x1 = Tmin + (Tb - Tmin) * 0.9
                                    x2 = Tmin + (Tb - Tmin) * 0.8
                                    x3 = Tmin + (Tb - Tmin) * 0.7
                                    x4 = Tmin + (Tb - Tmin) * 0.6
                                    x5 = Tmin + (Tb - Tmin) * 0.5
                                    p1 = CoolProp.PropsSI("L", "T", x1, "P", P, GetCoolPropName(sub1))
                                    p2 = CoolProp.PropsSI("L", "T", x2, "P", P, GetCoolPropName(sub1))
                                    p3 = CoolProp.PropsSI("L", "T", x3, "P", P, GetCoolPropName(sub1))
                                    p4 = CoolProp.PropsSI("L", "T", x4, "P", P, GetCoolPropName(sub1))
                                    p5 = CoolProp.PropsSI("L", "T", x5, "P", P, GetCoolPropName(sub1))
                                    vk(i) = MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
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
                End If
                If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                vk(i) = subst.MassFraction.GetValueOrDefault * vk(i)
                i = i + 1
            Next
            val = MathEx.Common.Sum(vk)

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Thermal Conductivity: {0} W/[m.K]", val))
            IObj?.Close()

            Return val
        End Function

        Public Overrides Function AUX_LIQDENS(T As Double, Vx As System.Array, Optional P As Double = 0.0, Optional Pvp As Double = 0.0, Optional FORCE_EOS As Boolean = False) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Liquid Density)")
            Inspector.Host.CheckAndAdd(IObj, "", "AUX_LIQDENS", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))

            Dim val As Double
            Dim i As Integer
            Dim Tmin, Tmax, Tc, Pmin, Pmax, Tb As Double
            Dim vk(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            i = 0
            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(1).Compounds.Values
                If Vx(i) > 0.0 Then
                    If IsCompoundSupported(1.0, subst.Name) Then
                        Dim sub1 As String = subst.Name
                        Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                        Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                        Tc = CoolProp.Props1SI(GetCoolPropName(sub1), "TCRIT")
                        Pmin = CoolProp.Props1SI(GetCoolPropName(sub1), "PMIN")
                        Pmax = CoolProp.Props1SI(GetCoolPropName(sub1), "PMAX")
                        If P > Pmin And P < Pmax Then
                            Tb = Me.AUX_TSATi(P, i)
                            If T < Tb And Abs(T - Tb) > 0.01 And T > Tmin Then
                                Try
                                    vk(i) = CoolProp.PropsSI("D", "T", T, "P", P, GetCoolPropName(sub1))
                                Catch ex As Exception
                                    WriteWarningMessage(ex.Message.ToString & ". Estimating value using Rackett [Fluid: " & subst.ConstantProperties.Name & "]")
                                    vk(i) = Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure,
                                                           subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight)
                                End Try
                            Else
                                Try
                                    vk(i) = CoolProp.PropsSI("D", "T", T, "Q", 0, GetCoolPropName(sub1))
                                Catch ex As Exception
                                    WriteWarningMessage(ex.Message.ToString & ". Estimating value using Rackett [Fluid: " & subst.ConstantProperties.Name & "]")
                                    vk(i) = Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure,
                                                           subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight)
                                End Try
                            End If
                        ElseIf T > Tmin And T <= Tmax And T <= Tc Then
                            Try
                                vk(i) = CoolProp.PropsSI("D", "T", T, "Q", 0, GetCoolPropName(sub1))
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
                End If
                If Vx(i) <> 0.0# Then vk(i) = Vx(i) / vk(i) Else vk(i) = 0.0
                If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                i = i + 1
            Next
            val = 1 / MathEx.Common.Sum(vk)
            If Double.IsNaN(val) Or Double.IsInfinity(val) Then val = 0.0#

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Density: {0} kg/m3", val))
            IObj?.Close()

            Return val
        End Function

        Public Overrides Function AUX_LIQDENSi(subst As Interfaces.ICompound, T As Double) As Double

            Return AUX_LIQDENSi(subst.ConstantProperties, T)

        End Function

        Public Overrides Function AUX_LIQTHERMCONDi(cprop As Interfaces.ICompoundConstantProperties, T As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Liquid Thermal Conductivity - {0})", cprop.Name)
            Inspector.Host.CheckAndAdd(IObj, "", "AUX_LIQTHERMCONDi", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))

            Dim sub1 = cprop.Name
            Dim Tmin, Tmax, Tc, val As Double
            If IsCompoundSupported(1.0, sub1) Then
                Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                Tc = CoolProp.Props1SI(GetCoolPropName(sub1), "TCRIT")
                If T > Tmin And T <= Tmax And T <= Tc Then
                    Try
                        val = CoolProp.PropsSI("L", "T", T, "Q", 0, GetCoolPropName(sub1)) * 1000
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

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Thermal Conductivity: {0} W/[m.K]", val))
            IObj?.Close()

            Return val
        End Function

        Public Overrides Function AUX_LIQVISCi(sub1 As String, T As Double, P As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Liquid Viscosity - {0})", sub1)
            Inspector.Host.CheckAndAdd(IObj, "", "AUX_LIQVISCi", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))

            Dim Tmin, Tmax, Tc, Pmin, Pmax, Tb, val As Double
            If IsCompoundSupported(1.0, sub1) Then
                Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                Tc = CoolProp.Props1SI(GetCoolPropName(sub1), "TCRIT")
                Pmin = CoolProp.Props1SI(GetCoolPropName(sub1), "PMIN")
                Pmax = CoolProp.Props1SI(GetCoolPropName(sub1), "PMAX")
                If P > Pmin And P < Pmax Then
                    Tb = Me.AUX_TSATi(P, sub1)
                    If T < Tb And Abs(T - Tb) > 0.01 And T > Tmin Then
                        Try
                            val = CoolProp.PropsSI("V", "T", T, "P", P, GetCoolPropName(sub1))
                        Catch ex As Exception
                            WriteWarningMessage(ex.Message.ToString & ". Estimating value... [Fluid: " & sub1 & "]")
                            val = MyBase.AUX_LIQVISCi(sub1, T, P)
                        End Try
                    Else
                        Try
                            val = CoolProp.PropsSI("V", "T", T, "Q", 0, GetCoolPropName(sub1))
                        Catch ex As Exception
                            WriteWarningMessage(ex.Message.ToString & ". Estimating value... [Fluid: " & sub1 & "]")
                            val = MyBase.AUX_LIQVISCi(sub1, T, P)
                        End Try
                    End If
                End If
            Else
                WriteWarningMessage("CoolProp Warning: compound " & sub1 & " not supported. Estimating Liquid Viscosity value...")
                val = MyBase.AUX_LIQVISCi(sub1, T, P)
            End If

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Viscosity: {0} Pa.s", val))
            IObj?.Close()

            Return val
        End Function

        Public Overrides Function AUX_SURFTi(constprop As Interfaces.ICompoundConstantProperties, T As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Liquid Surface Tension - {0})", constprop.Name)
            Inspector.Host.CheckAndAdd(IObj, "", "AUX_SURFTi", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))

            Dim sub1 = constprop.Name
            Dim Tmin, Tmax, Tc, val As Double
            If IsCompoundSupported(1.0, sub1) Then
                Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                Tc = CoolProp.Props1SI(GetCoolPropName(sub1), "TCRIT")
                If T > Tmin And T <= Tmax Then
                    Try
                        If T <= Tc Then
                            val = CoolProp.PropsSI("I", "T", T, "Q", 0, GetCoolPropName(sub1))
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

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Surface Tension: {0} N/m", val))
            IObj?.Close()

            Return val
        End Function

        Public Overrides Function AUX_SURFTM(T As Double) As Double

            Dim subst As Interfaces.ICompound
            Dim val As Double = 0
            For Each subst In Me.CurrentMaterialStream.Phases(1).Compounds.Values
                If subst.MassFraction.GetValueOrDefault() > 0.0 Then val += subst.MoleFraction.GetValueOrDefault * Me.AUX_SURFTi(subst.ConstantProperties, T)
            Next
            Return val

        End Function

        Public Overrides Function AUX_VAPTHERMCONDi(cprop As Interfaces.ICompoundConstantProperties, T As Double, P As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Vapor Thermal Coductivity - {0})", cprop.Name)
            Inspector.Host.CheckAndAdd(IObj, "", "AUX_VAPTHERMCONDi", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))

            Dim sub1 = cprop.Name
            Dim Tmin, Tmax, Pmin, Pmax, Tb, Tc, val As Double
            If IsCompoundSupported(1.0, sub1) Then
                Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                Pmin = CoolProp.Props1SI(GetCoolPropName(sub1), "PMIN")
                Pmax = CoolProp.Props1SI(GetCoolPropName(sub1), "PMAX")
                If T > Tmin And T < Tmax And P > Pmin And P < Pmax Then
                    Tc = CoolProp.Props1SI(cprop.Name, "TCRIT")
                    If T < Tc Then Tb = CoolProp.PropsSI("T", "P", P, "Q", 1, GetCoolPropName(sub1)) Else Tb = Tc
                    If T > Tb Then
                        Try
                            val = CoolProp.PropsSI("L", "T", T, "P", P, GetCoolPropName(sub1)) * 1000
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
                            p1 = CoolProp.PropsSI("L", "T", x1, "P", P, GetCoolPropName(sub1)) * 1000
                            p2 = CoolProp.PropsSI("L", "T", x2, "P", P, GetCoolPropName(sub1)) * 1000
                            p3 = CoolProp.PropsSI("L", "T", x3, "P", P, GetCoolPropName(sub1)) * 1000
                            p4 = CoolProp.PropsSI("L", "T", x4, "P", P, GetCoolPropName(sub1)) * 1000
                            p5 = CoolProp.PropsSI("L", "T", x5, "P", P, GetCoolPropName(sub1)) * 1000
                            val = MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
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

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Thermal Conductivity: {0} W/[m.K]", val))
            IObj?.Close()

            Return val
        End Function

        Public Overloads Function AUX_VAPVISCi(cprop As Interfaces.ICompoundConstantProperties, T As Double, P As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Vapor Viscosity - {0})", cprop.Name)
            Inspector.Host.CheckAndAdd(IObj, "", "AUX_VAPVISCi", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))

            Dim sub1 = cprop.Name
            Dim Tmin, Tmax, val As Double
            If IsCompoundSupported(1.0, sub1) Then
                Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                If T > Tmin And T < Tmax Then
                    Try
                        val = CoolProp.PropsSI("V", "T", T, "P", P, GetCoolPropName(sub1))
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

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Viscosity: {0} Pa.s", val))
            IObj?.Close()

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
                If subst.MassFraction.GetValueOrDefault() > 0.0 Then
                    If IsCompoundSupported(1.0, subst.Name) Then
                        Dim sub1 As String = subst.Name
                        Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                        Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                        Pmin = CoolProp.Props1SI(GetCoolPropName(sub1), "PMIN")
                        Pmax = CoolProp.Props1SI(GetCoolPropName(sub1), "PMAX")
                        If T > Tmin And T < Tmax And P > Pmin And P < Pmax Then
                            Tc = CoolProp.Props1SI(GetCoolPropName(sub1), "TCRIT")
                            If T < Tc Then Tb = CoolProp.PropsSI("T", "P", P, "Q", 1, GetCoolPropName(sub1)) Else Tb = Tc
                            If T > Tb And Abs(T - Tb) > 0.01 Then
                                Try
                                    vk(i) = CoolProp.PropsSI("V", "T", T, "P", P, GetCoolPropName(sub1))
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
                                    p1 = CoolProp.PropsSI("V", "T", x1, "P", P, GetCoolPropName(sub1))
                                    p2 = CoolProp.PropsSI("V", "T", x2, "P", P, GetCoolPropName(sub1))
                                    p3 = CoolProp.PropsSI("V", "T", x3, "P", P, GetCoolPropName(sub1))
                                    p4 = CoolProp.PropsSI("V", "T", x4, "P", P, GetCoolPropName(sub1))
                                    p5 = CoolProp.PropsSI("V", "T", x5, "P", P, GetCoolPropName(sub1))
                                    vk(i) = MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
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
                End If
                If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                vk(i) = subst.MoleFraction.GetValueOrDefault * vk(i)
                i = i + 1
            Next
            val = MathEx.Common.Sum(vk)
            Return val
        End Function

        Public Overrides Function AUX_LIQDENS(ByVal T As Double, Optional ByVal P As Double = 0.0, Optional ByVal Pvp As Double = 0.0, Optional ByVal phaseid As Integer = 3, Optional ByVal FORCE_EOS As Boolean = False) As Double

            Return AUX_LIQDENS(T, RET_VMOL(RET_PHASECODE(phaseid)), P, Pvp, False)

        End Function

        Public Overrides Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Vapor Density)")
            Inspector.Host.CheckAndAdd(IObj, "", "AUX_VAPDENS", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))

            Dim val As Double = 1.0
            Dim i As Integer
            Dim Tmin, Tmax, Pmin, Pmax, Tb, Tc As Double
            Dim vk(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            i = 0
            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                If subst.MassFraction.GetValueOrDefault() > 0.0 Then
                    If IsCompoundSupported(1.0, subst.Name) Then
                        Dim sub1 As String = subst.Name
                        Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                        Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                        Pmin = CoolProp.Props1SI(GetCoolPropName(sub1), "PMIN")
                        Pmax = CoolProp.Props1SI(GetCoolPropName(sub1), "PMAX")
                        If T > Tmin And T < Tmax And P > Pmin And P < Pmax Then
                            Tc = CoolProp.Props1SI(GetCoolPropName(sub1), "TCRIT")
                            If T < Tc Then Tb = CoolProp.PropsSI("T", "P", P, "Q", 1, GetCoolPropName(sub1)) Else Tb = Tc
                            If T > Tb And Abs(T - Tb) > 0.01 Then
                                Try
                                    vk(i) = CoolProp.PropsSI("D", "T", T, "P", P, GetCoolPropName(sub1))
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
                                    p1 = CoolProp.PropsSI("D", "T", x1, "P", P, GetCoolPropName(sub1))
                                    p2 = CoolProp.PropsSI("D", "T", x2, "P", P, GetCoolPropName(sub1))
                                    p3 = CoolProp.PropsSI("D", "T", x3, "P", P, GetCoolPropName(sub1))
                                    p4 = CoolProp.PropsSI("D", "T", x4, "P", P, GetCoolPropName(sub1))
                                    p5 = CoolProp.PropsSI("D", "T", x5, "P", P, GetCoolPropName(sub1))
                                    vk(i) = MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
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
                End If
                If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                vk(i) = subst.MoleFraction * vk(i)
                i = i + 1
            Next
            val = MathEx.Common.Sum(vk)

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Density: {0} kg/m3", val))
            IObj?.Close()

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
                        If subst.MassFraction.GetValueOrDefault() > 0.0 Then
                            If IsCompoundSupported(1.0, subst.Name) Then
                                Dim sub1 As String = subst.Name
                                Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                                Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                                Pmin = CoolProp.Props1SI(GetCoolPropName(sub1), "PMIN")
                                Pmax = CoolProp.Props1SI(GetCoolPropName(sub1), "PMAX")
                                Tc = CoolProp.Props1SI(GetCoolPropName(sub1), "TCRIT")
                                If T > Tmin And T < Tmax And P > Pmin And P < Pmax And T <= Tc Then
                                    If T < Tc Then
                                        Tb = AUX_TSATi(P, subst.ConstantProperties, T)
                                    Else
                                        Tb = Tc
                                    End If
                                    If T < Tb And Abs(T - Tb) > 0.01 And T > Tmin Then
                                        Try
                                            vk(i) = CoolProp.PropsSI("C", "T", T, "P", P, GetCoolPropName(sub1)) / 1000
                                        Catch ex As Exception
                                            vk(i) = CoolProp.PropsSI("C", "T", T, "Q", 0, GetCoolPropName(sub1)) / 1000
                                        End Try
                                    Else
                                        WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Liquid Cp, compound " &
                                                           subst.ConstantProperties.Name & ". Extrapolating curve to obtain a value...")
                                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                        x1 = Tmin + (Tb - Tmin) * 0.9
                                        x2 = Tmin + (Tb - Tmin) * 0.8
                                        x3 = Tmin + (Tb - Tmin) * 0.7
                                        x4 = Tmin + (Tb - Tmin) * 0.6
                                        x5 = Tmin + (Tb - Tmin) * 0.5
                                        p1 = CoolProp.PropsSI("C", "T", x1, "P", P, GetCoolPropName(sub1)) / 1000
                                        p2 = CoolProp.PropsSI("C", "T", x2, "P", P, GetCoolPropName(sub1)) / 1000
                                        p3 = CoolProp.PropsSI("C", "T", x3, "P", P, GetCoolPropName(sub1)) / 1000
                                        p4 = CoolProp.PropsSI("C", "T", x4, "P", P, GetCoolPropName(sub1)) / 1000
                                        p5 = CoolProp.PropsSI("C", "T", x5, "P", P, GetCoolPropName(sub1)) / 1000
                                        vk(i) = MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
                                    End If
                                Else
                                    WriteWarningMessage("CoolProp Warning: unable to calculate Cp for " & subst.ConstantProperties.Name & " at T = " & T & " K and P = " & P & " Pa.")
                                    vk(i) = 0.0#
                                End If
                            Else
                                WriteWarningMessage("CoolProp Warning: compound " & subst.ConstantProperties.Name & " not supported.")
                                vk(i) = 0.0#
                            End If
                        End If
                        If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                        vk(i) = subst.MassFraction.GetValueOrDefault * vk(i)
                        i = i + 1
                    Next
                Case Phase.Vapor
                    i = 0
                    For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(phaseID).Compounds.Values
                        If subst.MassFraction.GetValueOrDefault() > 0.0 Then
                            If IsCompoundSupported(1.0, subst.Name) Then
                                Dim sub1 As String = subst.Name
                                Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                                Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                                Pmin = CoolProp.Props1SI(GetCoolPropName(sub1), "PMIN")
                                Pmax = CoolProp.Props1SI(GetCoolPropName(sub1), "PMAX")
                                If T > Tmin And T < Tmax And P > Pmin And P < Pmax Then
                                    Tb = AUX_TSATi(P, subst.ConstantProperties, T)
                                    If T > Tb And Abs(T - Tb) > 0.01 Then
                                        Try
                                            vk(i) = CoolProp.PropsSI("C", "T", T, "P", P, GetCoolPropName(sub1)) / 1000
                                        Catch ex As Exception
                                            vk(i) = CoolProp.PropsSI("C", "T", T, "Q", 1, GetCoolPropName(sub1)) / 1000
                                        End Try
                                    Else
                                        WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Vapor Cp, compound " &
                                                        subst.ConstantProperties.Name & ". Extrapolating curve to obtain a value...")
                                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                        x1 = Tb + (Tmax - Tb) * 0.2
                                        x2 = Tb + (Tmax - Tb) * 0.4
                                        x3 = Tb + (Tmax - Tb) * 0.6
                                        x4 = Tb + (Tmax - Tb) * 0.8
                                        x5 = Tb + (Tmax - Tb) * 0.9
                                        p1 = CoolProp.PropsSI("C", "T", x1, "P", P, GetCoolPropName(sub1)) / 1000
                                        p2 = CoolProp.PropsSI("C", "T", x2, "P", P, GetCoolPropName(sub1)) / 1000
                                        p3 = CoolProp.PropsSI("C", "T", x3, "P", P, GetCoolPropName(sub1)) / 1000
                                        p4 = CoolProp.PropsSI("C", "T", x4, "P", P, GetCoolPropName(sub1)) / 1000
                                        p5 = CoolProp.PropsSI("C", "T", x5, "P", P, GetCoolPropName(sub1)) / 1000
                                        vk(i) = MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
                                    End If
                                Else
                                    WriteWarningMessage("CoolProp Warning: unable to calculate Cp for " & subst.ConstantProperties.Name & " at T = " & T & " K and P = " & P & " Pa.")
                                    vk(i) = 0.0#
                                End If
                            Else
                                WriteWarningMessage("CoolProp Warning: compound " & subst.ConstantProperties.Name & " not supported.")
                                vk(i) = 0.0#
                            End If
                        End If
                        If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                        vk(i) = subst.MassFraction.GetValueOrDefault * vk(i)
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
                        If subst.MassFraction.GetValueOrDefault() > 0.0 Then
                            If IsCompoundSupported(1.0, subst.Name) Then
                                Dim sub1 As String = subst.Name
                                Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                                Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                                Pmin = CoolProp.Props1SI(GetCoolPropName(sub1), "PMIN")
                                Pmax = CoolProp.Props1SI(GetCoolPropName(sub1), "PMAX")
                                Tc = CoolProp.Props1SI(GetCoolPropName(sub1), "TCRIT")
                                If T > Tmin And T < Tmax And P > Pmin And P < Pmax And T <= Tc Then
                                    Tb = AUX_TSATi(P, subst.ConstantProperties, T)
                                    If T < Tb And Abs(T - Tb) > 0.01 And T > Tmin Then
                                        Try
                                            vk(i) = CoolProp.PropsSI("O", "T", T, "P", P, GetCoolPropName(sub1)) / 1000
                                        Catch ex As Exception
                                            vk(i) = CoolProp.PropsSI("O", "T", T, "Q", 0, GetCoolPropName(sub1)) / 1000
                                        End Try
                                    Else
                                        WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Liquid Cv, compound " &
                                                         subst.ConstantProperties.Name & ". Extrapolating curve to obtain a value...")
                                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                        x1 = Tmin + (Tb - Tmin) * 0.9
                                        x2 = Tmin + (Tb - Tmin) * 0.8
                                        x3 = Tmin + (Tb - Tmin) * 0.7
                                        x4 = Tmin + (Tb - Tmin) * 0.6
                                        x5 = Tmin + (Tb - Tmin) * 0.5
                                        p1 = CoolProp.PropsSI("O", "T", x1, "P", P, GetCoolPropName(sub1)) / 1000
                                        p2 = CoolProp.PropsSI("O", "T", x2, "P", P, GetCoolPropName(sub1)) / 1000
                                        p3 = CoolProp.PropsSI("O", "T", x3, "P", P, GetCoolPropName(sub1)) / 1000
                                        p4 = CoolProp.PropsSI("O", "T", x4, "P", P, GetCoolPropName(sub1)) / 1000
                                        p5 = CoolProp.PropsSI("O", "T", x5, "P", P, GetCoolPropName(sub1)) / 1000
                                        vk(i) = MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
                                    End If
                                Else
                                    WriteWarningMessage("CoolProp Warning: unable to calculate Cv for " & subst.ConstantProperties.Name & " at T = " & T & " K and P = " & P & " Pa.")
                                    vk(i) = 0.0#
                                End If
                            Else
                                WriteWarningMessage("CoolProp Warning: compound " & subst.ConstantProperties.Name & " not supported.")
                                vk(i) = 0.0#
                            End If
                        End If
                        If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                        vk(i) = subst.MassFraction.GetValueOrDefault * vk(i)
                        i = i + 1
                    Next
                Case Phase.Vapor
                    i = 0
                    For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(phaseID).Compounds.Values
                        If subst.MassFraction.GetValueOrDefault() > 0.0 Then
                            If IsCompoundSupported(1.0, subst.Name) Then
                                Dim sub1 As String = subst.Name
                                Tmin = CoolProp.Props1SI(GetCoolPropName(sub1), "TMIN")
                                Tmax = CoolProp.Props1SI(GetCoolPropName(sub1), "TMAX")
                                Pmin = CoolProp.Props1SI(GetCoolPropName(sub1), "PMIN")
                                Pmax = CoolProp.Props1SI(GetCoolPropName(sub1), "PMAX")
                                If T > Tmin And T < Tmax And P > Pmin And P < Pmax Then
                                    Tb = AUX_TSATi(P, subst.ConstantProperties, T)
                                    If T > Tb And Abs(T - Tb) > 0.01 Then
                                        Try
                                            vk(i) = CoolProp.PropsSI("O", "T", T, "P", P, GetCoolPropName(sub1)) / 1000
                                        Catch ex As Exception
                                            vk(i) = CoolProp.PropsSI("O", "T", T, "Q", 1, GetCoolPropName(sub1)) / 1000
                                        End Try
                                    Else
                                        WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Vapor Cv, compound " &
                                                         subst.ConstantProperties.Name & ". Extrapolating curve to obtain a value...")
                                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                        x1 = Tb + (Tmax - Tb) * 0.2
                                        x2 = Tb + (Tmax - Tb) * 0.4
                                        x3 = Tb + (Tmax - Tb) * 0.6
                                        x4 = Tb + (Tmax - Tb) * 0.8
                                        x5 = Tb + (Tmax - Tb) * 0.9
                                        p1 = CoolProp.PropsSI("O", "T", x1, "P", P, GetCoolPropName(sub1)) / 1000
                                        p2 = CoolProp.PropsSI("O", "T", x2, "P", P, GetCoolPropName(sub1)) / 1000
                                        p3 = CoolProp.PropsSI("O", "T", x3, "P", P, GetCoolPropName(sub1)) / 1000
                                        p4 = CoolProp.PropsSI("O", "T", x4, "P", P, GetCoolPropName(sub1)) / 1000
                                        p5 = CoolProp.PropsSI("O", "T", x5, "P", P, GetCoolPropName(sub1)) / 1000
                                        vk(i) = MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
                                    End If
                                Else
                                    WriteWarningMessage("CoolProp Warning: unable to calculate Cv for " & subst.ConstantProperties.Name & " at T = " & T & " K and P = " & P & " Pa.")
                                    vk(i) = 0.0#
                                End If
                            Else
                                WriteWarningMessage("CoolProp Warning: compound " & subst.ConstantProperties.Name & " not supported.")
                                vk(i) = 0.0#
                            End If
                        End If
                        If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                        vk(i) = subst.MassFraction.GetValueOrDefault * vk(i)
                        i = i + 1
                    Next
            End Select

            val = MathEx.Common.Sum(vk)

            Return val

        End Function

        Public Overrides Function DW_CalcEnthalpy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            SetCPDebugLevel()

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Phase Enthalpy)")
            Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcEnthalpy", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", DirectCast(Vx, Double()).ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("State: {0}", [Enum].GetName(st.GetType, st)))

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
                        If Vx(i) > 0.0 Then
                            If IsCompoundSupported(1.0, vn(i)) Then
                                Tmin = CoolProp.Props1SI(GetCoolPropName(vn(i)), "TMIN")
                                Tmax = CoolProp.Props1SI(GetCoolPropName(vn(i)), "TMAX")
                                Pmin = CoolProp.Props1SI(GetCoolPropName(vn(i)), "PMIN")
                                Pmax = CoolProp.Props1SI(GetCoolPropName(vn(i)), "PMAX")
                                'If P > Pmin And P < Pmax Then
                                Tb = Me.AUX_TSATi(P, i)
                                If T < Tb And Abs(T - Tb) >= 0.01 And T > Tmin Then
                                    vk(i) = CoolProp.PropsSI("H", "T", T, "P", P, GetCoolPropName(vn(i))) / 1000
                                ElseIf Abs(T - Tb) < 0.01 Then
                                    Try
                                        vk(i) = CoolProp.PropsSI("H", "P", P, "Q", 0, GetCoolPropName(vn(i))) / 1000
                                    Catch ex As Exception
                                        vk(i) = CoolProp.PropsSI("H", "T", Tb * 0.99, "P", P, GetCoolPropName(vn(i))) / 1000
                                    End Try
                                Else
                                    WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Liquid Enthalpy, compound " &
                                                     vn(i) & ". Extrapolating curve to obtain a value...")
                                    Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                    If Abs(T - Tmin) > Abs(Tb - T) Then
                                        x1 = Tmin + (Tb - Tmin) * 0.5
                                        x2 = Tmin + (Tb - Tmin) * 0.4
                                        x3 = Tmin + (Tb - Tmin) * 0.3
                                        x4 = Tmin + (Tb - Tmin) * 0.2
                                        x5 = Tmin + (Tb - Tmin) * 0.1
                                    Else
                                        x1 = Tmin + (Tb - Tmin) * 0.9
                                        x2 = Tmin + (Tb - Tmin) * 0.8
                                        x3 = Tmin + (Tb - Tmin) * 0.7
                                        x4 = Tmin + (Tb - Tmin) * 0.6
                                        x5 = Tmin + (Tb - Tmin) * 0.5
                                    End If
                                    p1 = CoolProp.PropsSI("H", "T", x1, "P", P, GetCoolPropName(vn(i))) / 1000
                                    p2 = CoolProp.PropsSI("H", "T", x2, "P", P, GetCoolPropName(vn(i))) / 1000
                                    p3 = CoolProp.PropsSI("H", "T", x3, "P", P, GetCoolPropName(vn(i))) / 1000
                                    p4 = CoolProp.PropsSI("H", "T", x4, "P", P, GetCoolPropName(vn(i))) / 1000
                                    p5 = CoolProp.PropsSI("H", "T", x5, "P", P, GetCoolPropName(vn(i))) / 1000
                                    vk(i) = MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
                                End If
                                'Else
                                '    WriteWarningMessage("CoolProp Warning: unable to calculate Enthalpy for " & vn(i) & " at T = " & T & " K and P = " & P & " Pa.")
                                '    vk(i) = 0.0#
                                'End If
                            Else
                                WriteWarningMessage("CoolProp Warning: compound " & vn(i) & " not supported.")
                                vk(i) = 0.0#
                            End If
                        End If
                        If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                        vk(i) = Vxw(i) * vk(i)
                    Next
                Case State.Vapor
                    For i = 0 To n
                        If Vx(i) > 0.0 Then
                            If IsCompoundSupported(1.0, vn(i)) Then
                                Tmin = CoolProp.Props1SI(GetCoolPropName(vn(i)), "TMIN")
                                Tmax = CoolProp.Props1SI(GetCoolPropName(vn(i)), "TMAX")
                                Pmin = CoolProp.Props1SI(GetCoolPropName(vn(i)), "PMIN")
                                Pmax = CoolProp.Props1SI(GetCoolPropName(vn(i)), "PMAX")
                                'If P > Pmin And P < Pmax Then
                                Tb = Me.AUX_TSATi(P, i)
                                If T > Tb And Abs(T - Tb) > 0.01 Then
                                    Try
                                        vk(i) = CoolProp.PropsSI("H", "T", T, "P", P, GetCoolPropName(vn(i))) / 1000
                                    Catch ex As Exception
                                        If Abs(T - Tmin) < 0.05 Then
                                            vk(i) = CoolProp.PropsSI("H", "T", Tmin + 0.05, "P", P, GetCoolPropName(vn(i))) / 1000
                                        End If
                                    End Try
                                ElseIf Abs(T - Tb) < 0.01 Then
                                    Try
                                        vk(i) = CoolProp.PropsSI("H", "P", P, "Q", 1, GetCoolPropName(vn(i))) / 1000
                                    Catch ex As Exception
                                        vk(i) = CoolProp.PropsSI("H", "T", Tb * 1.01, "P", P, GetCoolPropName(vn(i))) / 1000
                                    End Try
                                Else
                                    WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Vapor Enthalpy, compound " &
                                                         vn(i) & ". Extrapolating curve to obtain a value...")
                                    Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                    x1 = Tb + 1.0 + T * 0.2
                                    x2 = Tb + 1.0 + T * 0.4
                                    x3 = Tb + 1.0 + T * 0.6
                                    x4 = Tb + 1.0 + T * 0.8
                                    x5 = Tb + 1.0 + T * 0.9
                                    p1 = CoolProp.PropsSI("H", "T", x1, "P", P, GetCoolPropName(vn(i))) / 1000
                                    p2 = CoolProp.PropsSI("H", "T", x2, "P", P, GetCoolPropName(vn(i))) / 1000
                                    p3 = CoolProp.PropsSI("H", "T", x3, "P", P, GetCoolPropName(vn(i))) / 1000
                                    p4 = CoolProp.PropsSI("H", "T", x4, "P", P, GetCoolPropName(vn(i))) / 1000
                                    p5 = CoolProp.PropsSI("H", "T", x5, "P", P, GetCoolPropName(vn(i))) / 1000
                                    vk(i) = MathNet.Numerics.Interpolate.RationalWithPoles(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
                                End If
                                'Else
                                '    WriteWarningMessage("CoolProp Warning: unable to calculate Enthalpy for " & vn(i) & " at T = " & T & " K and P = " & P & " Pa.")
                                '    vk(i) = 0.0#
                                'End If
                            Else
                                WriteWarningMessage("CoolProp Warning: compound " & vn(i) & " not supported.")
                                vk(i) = 0.0#
                            End If
                        End If
                        If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                        vk(i) = Vxw(i) * vk(i)
                    Next
                Case State.Solid
                    Dim Hl = DW_CalcEnthalpy(Vx, T, P, State.Liquid)
                    Dim Hfus = Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T)
                    Return Hl - Hfus
            End Select

            val = MathEx.Common.Sum(vk)

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Enthalpy: {0} kJ/kg", val))
            IObj?.Close()

            Return val

        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Return DW_CalcEnthalpy(Vx, T, P, st)

        End Function

        Public Overrides Function AUX_HFUSi(ByVal sub1 As String, ByVal T As Double)

            'return DHfus in kJ/kg
            Dim cpc = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties
            Dim Tfus = cpc.TemperatureOfFusion

            Return cpc.EnthalpyOfFusionAtTf * 1000 / cpc.Molar_Weight

        End Function

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            SetCPDebugLevel()

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Dim routinename As String = ComponentName & String.Format(" (Phase Entropy)")
            Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcEntropy", routinename, "", True)
            _IObj = IObj

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", DirectCast(Vx, Double()).ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("State: {0}", [Enum].GetName(st.GetType, st)))

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
                        If Vx(i) > 0.0 Then
                            If IsCompoundSupported(1.0, vn(i)) Then
                                Tmin = CoolProp.Props1SI(GetCoolPropName(vn(i)), "TMIN")
                                Tmax = CoolProp.Props1SI(GetCoolPropName(vn(i)), "TMAX")
                                Pmin = CoolProp.Props1SI(GetCoolPropName(vn(i)), "PMIN")
                                Pmax = CoolProp.Props1SI(GetCoolPropName(vn(i)), "PMAX")
                                'If P > Pmin And P < Pmax Then
                                Tb = Me.AUX_TSATi(P, i)
                                If T < Tb And Abs(T - Tb) >= 0.01 And T > Tmin Then
                                    vk(i) = CoolProp.PropsSI("S", "T", T, "P", P, GetCoolPropName(vn(i))) / 1000
                                ElseIf (T - Tb) < 0.01 Then
                                    Try
                                        vk(i) = CoolProp.PropsSI("S", "P", P, "Q", 0, GetCoolPropName(vn(i))) / 1000
                                    Catch ex As Exception
                                        vk(i) = CoolProp.PropsSI("S", "T", Tb * 0.99, "P", P, GetCoolPropName(vn(i))) / 1000
                                    End Try
                                Else
                                    WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Liquid Entropy, compound " &
                                                     vn(i) & ". Extrapolating curve to obtain a value...")
                                    Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                    If Abs(T - Tmin) > Abs(Tb - T) Then
                                        x1 = Tmin + (Tb - Tmin) * 0.5
                                        x2 = Tmin + (Tb - Tmin) * 0.4
                                        x3 = Tmin + (Tb - Tmin) * 0.3
                                        x4 = Tmin + (Tb - Tmin) * 0.2
                                        x5 = Tmin + (Tb - Tmin) * 0.1
                                    Else
                                        x1 = Tmin + (Tb - Tmin) * 0.9
                                        x2 = Tmin + (Tb - Tmin) * 0.8
                                        x3 = Tmin + (Tb - Tmin) * 0.7
                                        x4 = Tmin + (Tb - Tmin) * 0.6
                                        x5 = Tmin + (Tb - Tmin) * 0.5
                                    End If
                                    p1 = CoolProp.PropsSI("S", "T", x1, "P", P, GetCoolPropName(vn(i))) / 1000
                                    p2 = CoolProp.PropsSI("S", "T", x2, "P", P, GetCoolPropName(vn(i))) / 1000
                                    p3 = CoolProp.PropsSI("S", "T", x3, "P", P, GetCoolPropName(vn(i))) / 1000
                                    p4 = CoolProp.PropsSI("S", "T", x4, "P", P, GetCoolPropName(vn(i))) / 1000
                                    p5 = CoolProp.PropsSI("S", "T", x5, "P", P, GetCoolPropName(vn(i))) / 1000
                                    vk(i) = MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
                                End If
                                'Else
                                '    WriteWarningMessage("CoolProp Warning: unable to calculate Entropy for " & vn(i) & " at T = " & T & " K and P = " & P & " Pa.")
                                '    vk(i) = 0.0#
                                'End If
                            Else
                                WriteWarningMessage("CoolProp Warning: compound " & vn(i) & " not supported.")
                                vk(i) = 0.0#
                            End If
                        End If
                        If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                        vk(i) = Vxw(i) * vk(i)
                    Next
                Case State.Vapor
                    For i = 0 To n
                        If Vx(i) > 0.0 Then
                            If IsCompoundSupported(1.0, vn(i)) Then
                                Tmin = CoolProp.Props1SI(GetCoolPropName(vn(i)), "TMIN")
                                Tmax = CoolProp.Props1SI(GetCoolPropName(vn(i)), "TMAX")
                                Pmin = CoolProp.Props1SI(GetCoolPropName(vn(i)), "PMIN")
                                Pmax = CoolProp.Props1SI(GetCoolPropName(vn(i)), "PMAX")
                                'If P > Pmin And P < Pmax Then
                                Tb = Me.AUX_TSATi(P, i)
                                If T > Tb And Abs(T - Tb) > 0.01 Then
                                    vk(i) = CoolProp.PropsSI("S", "T", T, "P", P, GetCoolPropName(vn(i))) / 1000
                                ElseIf (T - Tb) < 0.01 Then
                                    Try
                                        vk(i) = CoolProp.PropsSI("S", "P", P, "Q", 1, GetCoolPropName(vn(i))) / 1000
                                    Catch ex As Exception
                                        vk(i) = CoolProp.PropsSI("S", "T", Tb * 1.01, "P", P, GetCoolPropName(vn(i))) / 1000
                                    End Try
                                Else
                                    WriteWarningMessage("CoolProp Warning: T and/or P is/are outside the valid range for calculation of Vapor Entropy, compound " &
                                                     vn(i) & ". Extrapolating curve to obtain a value...")
                                    Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                    x1 = Tb + 1.0 + T * 0.2
                                    x2 = Tb + 1.0 + T * 0.4
                                    x3 = Tb + 1.0 + T * 0.6
                                    x4 = Tb + 1.0 + T * 0.8
                                    x5 = Tb + 1.0 + T * 0.9
                                    p1 = CoolProp.PropsSI("S", "T", x1, "P", P, GetCoolPropName(vn(i))) / 1000
                                    p2 = CoolProp.PropsSI("S", "T", x2, "P", P, GetCoolPropName(vn(i))) / 1000
                                    p3 = CoolProp.PropsSI("S", "T", x3, "P", P, GetCoolPropName(vn(i))) / 1000
                                    p4 = CoolProp.PropsSI("S", "T", x4, "P", P, GetCoolPropName(vn(i))) / 1000
                                    p5 = CoolProp.PropsSI("S", "T", x5, "P", P, GetCoolPropName(vn(i))) / 1000
                                    vk(i) = MathNet.Numerics.Interpolate.RationalWithPoles(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
                                End If
                                'Else
                                '    WriteWarningMessage("CoolProp Warning: unable to calculate Enthalpy for " & vn(i) & " at T = " & T & " K and P = " & P & " Pa.")
                                '    vk(i) = 0.0#
                                'End If
                            Else
                                WriteWarningMessage("CoolProp Warning: compound " & vn(i) & " not supported.")
                                vk(i) = 0.0#
                            End If
                        End If
                        If Double.IsNaN(vk(i)) Or Double.IsInfinity(vk(i)) Then vk(i) = 0.0#
                        vk(i) = Vxw(i) * vk(i)
                    Next
                Case State.Solid
                    Return DW_CalcEntropy(Vx, T, P, State.Liquid) - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
            End Select

            val = MathEx.Common.Sum(vk)

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Entropy: {0} kJ/[kg.K]", val))
            IObj?.Close()

            Return val

        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Return DW_CalcEntropy(Vx, T, P, st)

        End Function

        Public Overrides Function DW_CalcFugCoeff(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

            SetCPDebugLevel()

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcFugCoeff", "Fugacity Coefficient", "Property Package Fugacity Coefficient Calculation Routine")

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", DirectCast(Vx, Double()).ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("State: {0}", [Enum].GetName(st.GetType, st)))

            Calculator.WriteToConsole(Me.ComponentName & " fugacity coefficient calculation for phase '" & st.ToString & "' requested at T = " & T & " K and P = " & P & " Pa.", 2)
            Calculator.WriteToConsole("Compounds: " & Me.RET_VNAMES.ToArrayString, 2)
            Calculator.WriteToConsole("Mole fractions: " & Vx.ToArrayString(), 2)

            Dim n As Integer = Vx.Length - 1
            Dim i As Integer
            Dim fugcoeff(n) As Double

            If st = State.Liquid Then
                Dim Tc As Double() = Me.RET_VTC()
                For i = 0 To n
                    If Vx(i) > 0.0 Then
                        If T / Tc(i) >= 1 Then
                            IObj?.SetCurrent()
                            If UseHenryConstants And HasHenryConstants(RET_VNAMES(i)) Then
                                Dim hc = AUX_KHenry(RET_VNAMES(i), T)
                                IObj?.Paragraphs.Add(String.Format("Henry's Constant (H) @ {0} K: {1} Pa", T, hc))
                                fugcoeff(i) = hc / P
                            Else
                                fugcoeff(i) = Me.AUX_PVAPi(i, T) / P
                            End If
                        Else
                            IObj?.SetCurrent()
                            fugcoeff(i) = Me.AUX_PVAPi(i, T) / P
                        End If
                    Else
                        fugcoeff(i) = MyBase.AUX_PVAPi(RET_VNAMES(i), T)
                    End If
                Next
            Else
                For i = 0 To n
                    fugcoeff(i) = 1
                Next
            End If

            Calculator.WriteToConsole("Result: " & fugcoeff.ToArrayString(), 2)

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))
            IObj?.Paragraphs.Add(String.Format("Fugacity Coefficients: {0}", fugcoeff.ToMathArrayString))
            IObj?.Close()

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

            SetCPDebugLevel()

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcPhaseProps", ComponentName & String.Format(" (Phase Properties - {0})", [Enum].GetName(Phase.GetType, Phase)), "Property Package Phase Properties Calculation Routine")

            IObj?.Paragraphs.Add("This is the routine responsible for the calculation of phase properties of the currently associated Material Stream.")

            IObj?.Paragraphs.Add("Specified Phase: " & [Enum].GetName(Phase.GetType, Phase))

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
                IObj?.SetCurrent
                Me.DW_CalcCompVolFlow(phaseID)
                IObj?.SetCurrent
                Me.DW_CalcCompFugCoeff(Phase)
            End If

            If phaseID = 3 Or phaseID = 4 Or phaseID = 5 Or phaseID = 6 Then


                IObj?.SetCurrent
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = AUX_LIQDENS(T, P, 0.0#, phaseID)

                IObj?.SetCurrent
                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result

                IObj?.SetCurrent
                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result

                IObj?.SetCurrent
                result = P / (Me.AUX_LIQDENS(T, P, 0, phaseID) * 8.314 * T) / 1000 * AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result

                IObj?.SetCurrent
                result = Me.DW_CalcCp_ISOL(Phase, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result

                IObj?.SetCurrent
                result = Me.DW_CalcCv_ISOL(Phase, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result

                result = Me.AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result

                IObj?.SetCurrent
                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid) * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result

                IObj?.SetCurrent
                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid) * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result

                IObj?.SetCurrent
                result = Me.AUX_CONDTL(T, phaseID)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result

                IObj?.SetCurrent
                result = Me.AUX_LIQVISCm(T, P, phaseID)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result

                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

            ElseIf phaseID = 2 Then

                IObj?.SetCurrent
                result = Me.AUX_VAPDENS(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result

                IObj?.SetCurrent
                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result

                IObj?.SetCurrent
                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result

                IObj?.SetCurrent
                result = P / (Me.AUX_VAPDENS(T, P) * 8.314 * T) / 1000 * AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result

                IObj?.SetCurrent
                result = Me.DW_CalcCp_ISOL(Phase, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result

                IObj?.SetCurrent
                result = Me.DW_CalcCv_ISOL(Phase, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result

                IObj?.SetCurrent
                result = Me.AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result

                IObj?.SetCurrent
                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor) * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result

                IObj?.SetCurrent
                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor) * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result

                IObj?.SetCurrent
                result = Me.AUX_CONDTG(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result

                IObj?.SetCurrent
                result = Me.AUX_VAPVISCMIX(T, P, Me.AUX_MMM(Phase))
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault

            ElseIf phaseID = 1 Then

                IObj?.SetCurrent
                DW_CalcLiqMixtureProps()


            Else

                IObj?.SetCurrent
                DW_CalcOverallProps()

            End If


            If phaseID > 0 Then
                result = overallmolarflow * phasemolarfrac * Me.AUX_MMM(Phase) / 1000 / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = result
            End If

            IObj?.Close()

        End Sub

        Public Overrides Sub DW_CalcProp(ByVal [property] As String, ByVal phase As Phase)

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
                Case "isothermalcompressibility", "bulkmodulus", "joulethomsoncoefficient", "speedofsound", "internalenergy", "gibbsenergy", "helmholtzenergy"
                    CalcAdditionalPhaseProperties(phaseID)
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
                        result = Me.AUX_LIQVISCm(T, P, phaseID)
                    Else
                        result = Me.AUX_VAPVISCMIX(T, P, Me.AUX_MMM(phase))
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Case "thermalconductivity"
                    If state = "L" Then
                        result = Me.AUX_CONDTL(T, phaseID)
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
                Return Me.AUX_LIQVISCm(T, P)
            ElseIf Phase1 = Phase.Vapor Then
                Return Me.AUX_VAPVISCm(T, Me.AUX_VAPDENS(T, P), Me.AUX_MMM(Phase.Vapor))
            End If

        End Function

        Public Overrides Function SupportsComponent(comp As Interfaces.ICompoundConstantProperties) As Boolean

            Return IsCompoundSupported(1.0, comp.Name)

        End Function

        Public Overrides Function DW_CalcEnergyFlowMistura_ISOL(T As Double, P As Double) As Double

        End Function

#End Region

#Region "    Auxiliary Functions"

        Function IsCompoundSupported(x As Double, compname As String) As Boolean

            If x = 0.0 Then
                Return True
            Else
                If SupportedComponents.Contains(compname) Then
                    Return True
                Else
                    If CompoundAliases.ContainsKey(CurrentMaterialStream.Phases(0).Compounds(compname).ConstantProperties.CAS_Number) Then
                        Return True
                    Else
                        Dim e1 = New ArgumentOutOfRangeException(compname, "Error: compound '" & compname & "' is not supported by this version of CoolProp.")
                        e1.Data.Add("DetailedDescription", "CoolProp doesn't suppoert his compound, so the calculation results will be incorrect.")
                        e1.Data.Add("UserAction", "Remove the compound from the simulation or try another Property Package.")
                        Throw e1
                    End If
                End If
            End If

        End Function

#End Region

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

        Public Overrides Function AUX_Z(Vx() As Double, T As Double, P As Double, state As PhaseName) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "AUX_Z", "Compressibility Factor", "Compressibility Factor Calculation Routine")

            IObj?.SetCurrent()

            Dim val As Double
            If state = PhaseName.Liquid Then
                val = P / (Me.AUX_LIQDENS(T, Vx, P) * 8.314 * T) / 1000 * AUX_MMM(Vx)
            Else
                val = P / (Me.AUX_VAPDENS(T, P) * 8.314 * T) / 1000 * AUX_MMM(Vx)
            End If


            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("Compressibility Factor: {0}", val))

            IObj?.Close()

            Return val

        End Function

    End Class

End Namespace
