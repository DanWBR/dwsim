Imports System.IO
Imports Python.Runtime
Imports DWSIM.ExtensionMethods

Public Class ChEDLThermoParser

    Function SearchCompound(searchtext As String) As List(Of String)

        GlobalSettings.Settings.InitializePythonEnvironment()

        Dim sList As New List(Of String)

        Using Py.GIL()

            Dim identf As Object = Py.Import("chemicals.identifiers")

            Dim result As Object = identf.search_chemical(searchtext)

            If result IsNot Nothing Then sList.Add(result.common_name.ToString())

        End Using

        Return sList

    End Function

    Function GetCompoundData(common_name As String) As BaseClasses.ConstantProperties

        GlobalSettings.Settings.InitializePythonEnvironment()

        Using Py.GIL

            Dim ci As System.Globalization.CultureInfo = New Globalization.CultureInfo("en-US")

            Dim comp As New BaseClasses.ConstantProperties

            comp.Comments = "Data from ChEDL Thermo Python Library (https://github.com/CalebBell/thermo)"

            comp.OriginalDB = "ChEDL Thermo"

            comp.CurrentDB = "ChEDL Thermo"

            'string properties

            comp.ID = New Random().Next(700001, 799999)

            Dim identf As Object = Py.Import("chemicals.identifiers")

            Dim result As Object = identf.IDs_to_CASs(common_name)

            Dim CAS = result(0).ToString()

            ' meta

            Dim meta As Object = identf.search_chemical(common_name)

            comp.Name = common_name
            comp.Molar_Weight = (meta.MW.ToString().ToDoubleFromInvariant())
            comp.InChI = meta.InChI.ToString()
            comp.SMILES = meta.smiles.ToString()
            comp.CAS_Number = CAS.ToString()
            comp.Formula = meta.formula.ToString()

            'critical

            Dim instance As Object = Py.Import("chemicals.acentric")

            comp.Acentric_Factor = (instance.omega(CAS).ToString().ToDoubleFromInvariant())

            instance = Py.Import("chemicals.critical")

            comp.Critical_Temperature = (instance.Tc(CAS).ToString().ToDoubleFromInvariant())
            comp.Critical_Pressure = (instance.Pc(CAS).ToString().ToDoubleFromInvariant())
            comp.Critical_Volume = (instance.Vc(CAS).ToString().ToDoubleFromInvariant())
            comp.Critical_Compressibility = (instance.Zc(CAS).ToString().ToDoubleFromInvariant())
            comp.Z_Rackett = comp.Critical_Compressibility

            ' phase change

            instance = Py.Import("chemicals.phase_change")

            comp.Normal_Boiling_Point = (instance.Tb(CAS).ToString().ToDoubleFromInvariant())
            comp.NBP = comp.Normal_Boiling_Point
            Try
                comp.TemperatureOfFusion = (instance.Tm(CAS).ToString().ToDoubleFromInvariant())
            Catch ex As Exception
            End Try
            Try
                comp.EnthalpyOfFusionAtTf = (instance.Hfus(CAS).ToString().ToDoubleFromInvariant())
            Catch ex As Exception
            End Try

            ' formation data

            instance = Py.Import("chemicals.reaction")

            Try
                comp.IG_Enthalpy_of_Formation_25C = (instance.Hfg(CAS).ToString().ToDoubleFromInvariant()) / comp.Molar_Weight
                comp.IG_Entropy_of_Formation_25C = (instance.S0g(CAS).ToString().ToDoubleFromInvariant()) / comp.Molar_Weight
                comp.IG_Gibbs_Energy_of_Formation_25C = comp.IG_Enthalpy_of_Formation_25C - 298.15 * comp.IG_Entropy_of_Formation_25C
            Catch ex As Exception
            End Try

            'comp.Dipole_Moment = Python.GetScalar("print c.dipole")

            'model-specific
            'comp.UNIQUAC_R = Python.GetScalar("print c.UNIFAC_R")
            'comp.UNIQUAC_Q = Python.GetScalar("print c.UNIFAC_Q")

            'temperature-dependent data setup

            Dim lmfit As New LMFit

            Dim TrangeS, TrangeL, TrangeV As New List(Of Double), Tmin, Tb, Tmax, Tx As Double
            Tmin = comp.TemperatureOfFusion
            Tb = comp.Normal_Boiling_Point
            Tmax = comp.Critical_Temperature

            If Tmin = 0.0# Then Tmin = Tb * 0.3

            For Tx = Tmin * 0.5 To Tmin Step (0.5 * Tmin) / 25
                TrangeS.Add(Tx)
            Next

            For Tx = Tmin To Tb Step (Tb - Tmin) / 100
                TrangeL.Add(Tx)
            Next

            For Tx = Tb To Tmax Step (Tmax - Tb) / 100
                TrangeV.Add(Tx)
            Next

            Dim Pvap, CpIG, CpL, CpS, TCL, TCV, DensL, DensS, ViscL, ViscV, Hvap, SurfT As New List(Of Double)
            Dim coeffs(4) As Double, obj As Object, fitcoeffs As Double(), r_fit, n_fit As Double

            'vapor pressure

            Dim value As Double

            instance = Py.Import("thermo.vapor_pressure")

            Dim calculator As Object = instance.VaporPressure(CASRN:=CAS, extrapolation:="interp1d")

            Try

                Dim tvals = New List(Of Double)
                For Each item In TrangeL
                    Dim p = calculator.T_dependent_property(item.ToPython())
                    If p IsNot Nothing Then
                        value = p.ToString().ToDoubleFromInvariant()
                        Pvap.Add(value)
                        tvals.Add(item)
                    End If
                Next

                comp.VaporPressureEquation = 101

                coeffs(0) = 74.555
                coeffs(1) = -7295.59
                coeffs(2) = -7.44245
                coeffs(3) = 0.0000042881
                coeffs(4) = 2.0#

                'obj = lmfit.GetCoeffs(tvals.ToArray, Pvap.ToArray, coeffs.Clone, LMFit.FitType.Pvap, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                'fitcoeffs = obj(0)
                'r_fit = obj(2)
                'n_fit = obj(3)

                obj = lmfit.GetCoeffs(tvals.ToArray, Pvap.ToArray, coeffs.Clone, 0.00000001, 10000,
                                      Function(c, x)
                                          Dim val = PropertyPackages.PropertyPackage.CalcCSTDepProp(101, c(1), c(2), c(3), c(4), c(5), x, 0)
                                          Return val
                                      End Function)
                fitcoeffs = obj(0)
                r_fit = obj(2)
                n_fit = obj(3)

                comp.Vapor_Pressure_Constant_A = fitcoeffs(0)
                comp.Vapor_Pressure_Constant_B = fitcoeffs(1)
                comp.Vapor_Pressure_Constant_C = fitcoeffs(2)
                comp.Vapor_Pressure_Constant_D = fitcoeffs(3)
                comp.Vapor_Pressure_Constant_E = fitcoeffs(4)

                comp.Vapor_Pressure_TMIN = Tmin
                comp.Vapor_Pressure_TMAX = Tb

                Dim ycalc = tvals.Select(Function(x) comp.GetVaporPressure(x)).ToList()

                comp.Comments += vbCrLf
                comp.Comments += "Vapor Pressure regression residual = " + r_fit.ToString(ci) + vbCrLf
                comp.Comments += "Regressed Data Table" + vbCrLf
                comp.Comments += GetTable(tvals, Pvap, ycalc, "T (K)", "Pvap (Pa)")

                comp.Vapor_Pressure_Regression_Fit = r_fit
                comp.Vapor_Pressure_Tabular_Data.XData = tvals
                comp.Vapor_Pressure_Tabular_Data.YData = Pvap
                comp.Vapor_Pressure_Tabular_Data.XName = "Temperature"
                comp.Vapor_Pressure_Tabular_Data.YName = "Vapor Pressure"
                comp.Vapor_Pressure_Tabular_Data.XUnit = "K"
                comp.Vapor_Pressure_Tabular_Data.YUnit = "Pa"

            Catch ex As Exception

            End Try

            'ideal gas cp

            Try

                instance = Py.Import("thermo.heat_capacity")
                calculator = instance.HeatCapacityGas(CASRN:=CAS, extrapolation:="interp1d")

                Dim tvals = New List(Of Double)

                For Each item In TrangeV
                    Dim p = calculator.T_dependent_property(item.ToPython())
                    If p IsNot Nothing Then
                        value = p.ToString().ToDoubleFromInvariant()
                        CpIG.Add(value / comp.Molar_Weight)
                        tvals.Add(item)
                    End If
                Next

                comp.IdealgasCpEquation = 5

                coeffs(0) = 1.0
                coeffs(1) = 0.000000249
                coeffs(2) = 0.00000000253
                coeffs(3) = 0.0
                coeffs(4) = 0.0

                obj = lmfit.GetCoeffs(tvals.ToArray, CpIG.ToArray, coeffs.Clone, LMFit.FitType.Cp, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                fitcoeffs = obj(0)
                r_fit = obj(2)
                n_fit = obj(3)

                comp.Ideal_Gas_Heat_Capacity_Const_A = fitcoeffs(0)
                comp.Ideal_Gas_Heat_Capacity_Const_B = fitcoeffs(1)
                comp.Ideal_Gas_Heat_Capacity_Const_C = fitcoeffs(2)
                comp.Ideal_Gas_Heat_Capacity_Const_D = fitcoeffs(3)
                comp.Ideal_Gas_Heat_Capacity_Const_E = fitcoeffs(4)

                Dim ycalc = tvals.Select(Function(x) comp.GetIdealGasHeatCapacity(x)).ToList()

                comp.Comments += vbCrLf
                comp.Comments += "Ideal Gas Heat Capacity regression residual = " + r_fit.ToString(ci) + vbCrLf
                comp.Comments += "Regressed Data Table" + vbCrLf
                comp.Comments += GetTable(tvals, CpIG, ycalc, "T (K)", "Cp (kJ/kg.K)")

                comp.Ideal_Gas_Heat_Capacity_Regression_Fit = r_fit
                comp.Ideal_Gas_Heat_Capacity_Tabular_Data.XData = tvals
                comp.Ideal_Gas_Heat_Capacity_Tabular_Data.YData = CpIG
                comp.Ideal_Gas_Heat_Capacity_Tabular_Data.XName = "Temperature"
                comp.Ideal_Gas_Heat_Capacity_Tabular_Data.YName = "Heat Capacity"
                comp.Ideal_Gas_Heat_Capacity_Tabular_Data.XUnit = "K"
                comp.Ideal_Gas_Heat_Capacity_Tabular_Data.YUnit = "kJ/[kg.K]"

            Catch ex As Exception

            End Try

            'liquid cp

            Try

                calculator = instance.HeatCapacityLiquid(CASRN:=CAS, extrapolation:="interp1d")

                For Each item In TrangeL
                    Dim p = calculator.T_dependent_property(item.ToPython())
                    If p IsNot Nothing Then
                        value = p.ToString().ToDoubleFromInvariant()
                        CpL.Add(value / comp.Molar_Weight)
                    End If
                Next

                If Not CpL.Sum = 0.0# Then

                    comp.LiquidHeatCapacityEquation = 5

                    coeffs(0) = 33.7
                    coeffs(1) = 0.249
                    coeffs(2) = 0.000253
                    coeffs(3) = -0.000000384
                    coeffs(4) = 0.000000000129

                    obj = lmfit.GetCoeffs(TrangeL.ToArray, CpL.ToArray, coeffs.Clone, ChEDLThermoLink.LMFit.FitType.Cp, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                    fitcoeffs = obj(0)
                    r_fit = obj(2)
                    n_fit = obj(3)

                    comp.Liquid_Heat_Capacity_Const_A = fitcoeffs(0)
                    comp.Liquid_Heat_Capacity_Const_B = fitcoeffs(1)
                    comp.Liquid_Heat_Capacity_Const_C = fitcoeffs(2)
                    comp.Liquid_Heat_Capacity_Const_D = fitcoeffs(3)
                    comp.Liquid_Heat_Capacity_Const_E = fitcoeffs(4)

                    comp.Liquid_Heat_Capacity_Tmin = Tmin
                    comp.Liquid_Heat_Capacity_Tmax = Tb

                    Dim ycalc = TrangeL.Select(Function(x) comp.GetLiquidHeatCapacity(x)).ToList()

                    comp.Comments += vbCrLf
                    comp.Comments += "Liquid Heat Capacity regression residual = " + r_fit.ToString(ci) + vbCrLf
                    comp.Comments += "Regressed Data Table" + vbCrLf
                    comp.Comments += GetTable(TrangeL, CpL, ycalc, "T (K)", "Cp (kJ/kg.K)")

                    comp.Liquid_Heat_Capacity_Regression_Fit = r_fit
                    comp.Liquid_Heat_Capacity_Tabular_Data.XData = TrangeL
                    comp.Liquid_Heat_Capacity_Tabular_Data.YData = CpL
                    comp.Liquid_Heat_Capacity_Tabular_Data.XName = "Temperature"
                    comp.Liquid_Heat_Capacity_Tabular_Data.YName = "Heat Capacity"
                    comp.Liquid_Heat_Capacity_Tabular_Data.XUnit = "K"
                    comp.Liquid_Heat_Capacity_Tabular_Data.YUnit = "kJ/[kg.K]"

                Else

                    comp.Comments += vbCrLf
                    comp.Comments += "Liquid Heat Capacity data unavailable, will be estimated." + vbCrLf

                End If

            Catch ex As Exception

                comp.Comments += vbCrLf
                comp.Comments += "Liquid Heat Capacity data unavailable, will be estimated." + vbCrLf

            End Try

            'solid cp

            Try

                calculator = instance.HeatCapacitySolid(CASRN:=CAS, extrapolation:="interp1d")

                For Each item In TrangeS
                    Dim p = calculator.T_dependent_property(item.ToPython())
                    If p IsNot Nothing Then
                        value = p.ToString().ToDoubleFromInvariant()
                        CpS.Add(value / comp.Molar_Weight)
                    End If
                Next

                If Not CpS.Sum = 0 Then

                    comp.SolidHeatCapacityEquation = 5

                    coeffs(0) = 33.7
                    coeffs(1) = 0.249
                    coeffs(2) = 0.000253
                    coeffs(3) = -0.000000384
                    coeffs(4) = 0.000000000129

                    obj = lmfit.GetCoeffs(TrangeS.ToArray, CpS.ToArray, coeffs.Clone, ChEDLThermoLink.LMFit.FitType.Cp, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                    fitcoeffs = obj(0)
                    r_fit = obj(2)
                    n_fit = obj(3)

                    comp.Solid_Heat_Capacity_Const_A = fitcoeffs(0)
                    comp.Solid_Heat_Capacity_Const_B = fitcoeffs(1)
                    comp.Solid_Heat_Capacity_Const_C = fitcoeffs(2)
                    comp.Solid_Heat_Capacity_Const_D = fitcoeffs(3)
                    comp.Solid_Heat_Capacity_Const_E = fitcoeffs(4)

                    comp.Solid_Heat_Capacity_Tmin = Tmin * 0.5
                    comp.Solid_Heat_Capacity_Tmax = Tmin

                    Dim ycalc = TrangeS.Select(Function(x) comp.GetSolidHeatCapacity(x)).ToList()

                    comp.Comments += vbCrLf
                    comp.Comments += "Solid Heat Capacity regression residual = " + r_fit.ToString(ci) + vbCrLf
                    comp.Comments += "Regressed Data Table" + vbCrLf
                    comp.Comments += GetTable(TrangeS, CpS, ycalc, "T (K)", "Cp (kJ/kg.K)")

                    comp.Solid_Heat_Capacity_Regression_Fit = r_fit
                    comp.Solid_Heat_Capacity_Tabular_Data.XData = TrangeS
                    comp.Liquid_Heat_Capacity_Tabular_Data.YData = CpS
                    comp.Solid_Heat_Capacity_Tabular_Data.XName = "Temperature"
                    comp.Solid_Heat_Capacity_Tabular_Data.YName = "Heat Capacity"
                    comp.Solid_Heat_Capacity_Tabular_Data.XUnit = "K"
                    comp.Solid_Heat_Capacity_Tabular_Data.YUnit = "kJ/[kg.K]"

                Else

                    comp.Comments += vbCrLf
                    comp.Comments += "Solid Heat Capacity data unavailable, will be estimated." + vbCrLf

                End If

            Catch ex As Exception

                comp.Comments += vbCrLf
                comp.Comments += "Solid Heat Capacity data unavailable, will be estimated." + vbCrLf

            End Try

            'solid density

            Try

                instance = Py.Import("thermo.volume")
                calculator = instance.VolumeSolid(CASRN:=CAS, extrapolation:="interp1d")

                For Each item In TrangeS
                    Dim p = calculator.T_dependent_property(item.ToPython())
                    If p IsNot Nothing Then
                        value = p.ToString().ToDoubleFromInvariant()
                        DensS.Add(1 / value / 1000 * comp.Molar_Weight)
                    End If
                Next

                If Not DensS.Sum = Double.PositiveInfinity Then

                    comp.SolidDensityEquation = 5

                    coeffs(0) = 11
                    coeffs(1) = -0.005
                    coeffs(2) = 0.0#
                    coeffs(3) = 0.0#
                    coeffs(4) = 0.0#

                    obj = lmfit.GetCoeffs(TrangeS.ToArray, DensS.ToArray, coeffs.Clone, ChEDLThermoLink.LMFit.FitType.Cp, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                    fitcoeffs = obj(0)
                    r_fit = obj(2)
                    n_fit = obj(3)

                    comp.Solid_Density_Const_A = fitcoeffs(0)
                    comp.Solid_Density_Const_B = fitcoeffs(1)
                    comp.Solid_Density_Const_C = fitcoeffs(2)
                    comp.Solid_Density_Const_D = fitcoeffs(3)
                    comp.Solid_Density_Const_E = fitcoeffs(4)

                    comp.Solid_Density_Tmin = Tmin * 0.5
                    comp.Solid_Density_Tmax = Tmin

                    Dim ycalc = TrangeS.Select(Function(x) comp.GetSolidDensity(x)).ToList()

                    comp.Comments += vbCrLf
                    comp.Comments += "Solid Density regression residual = " + r_fit.ToString(ci) + vbCrLf
                    comp.Comments += "Regressed Data Table" + vbCrLf
                    comp.Comments += GetTable(TrangeS, DensS, ycalc, "T (K)", "rhoS (kg/m3)")

                    comp.Solid_Density_Regression_Fit = r_fit
                    comp.Solid_Density_Tabular_Data.XData = TrangeS
                    comp.Solid_Density_Tabular_Data.YData = DensS
                    comp.Solid_Density_Tabular_Data.XName = "Temperature"
                    comp.Solid_Density_Tabular_Data.YName = "Density"
                    comp.Solid_Density_Tabular_Data.XUnit = "K"
                    comp.Solid_Density_Tabular_Data.YUnit = "kg/m3"

                Else

                    comp.Comments += vbCrLf
                    comp.Comments += "Solid Density data unavailable, will be estimated." + vbCrLf

                End If

            Catch ex As Exception

                comp.Comments += vbCrLf
                comp.Comments += "Solid Density data unavailable, will be estimated." + vbCrLf

            End Try

            'liquid density

            Try

                calculator = instance.VolumeLiquid(CASRN:=CAS, extrapolation:="interp1d")

                For Each item In TrangeL
                    Dim p = calculator.TP_or_T_dependent_property(item.ToPython(), 101325.0F.ToPython())
                    If p IsNot Nothing Then
                        value = p.ToString().ToDoubleFromInvariant()
                        DensL.Add(1 / value / 1000 * comp.Molar_Weight)
                    End If
                Next

                If Not DensL.Sum = Double.PositiveInfinity Then

                    comp.LiquidDensityEquation = 5

                    coeffs(0) = 800.0
                    coeffs(1) = -0.249
                    coeffs(2) = 0.000253
                    coeffs(3) = -0.000000384
                    coeffs(4) = 0.000000000129

                    obj = lmfit.GetCoeffs(TrangeL.ToArray, DensL.ToArray, coeffs.Clone, ChEDLThermoLink.LMFit.FitType.Cp, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                    fitcoeffs = obj(0)
                    r_fit = obj(2)
                    n_fit = obj(3)

                    comp.Liquid_Density_Const_A = fitcoeffs(0)
                    comp.Liquid_Density_Const_B = fitcoeffs(1)
                    comp.Liquid_Density_Const_C = fitcoeffs(2)
                    comp.Liquid_Density_Const_D = fitcoeffs(3)
                    comp.Liquid_Density_Const_E = fitcoeffs(4)

                    comp.Liquid_Density_Tmin = Tmin
                    comp.Liquid_Density_Tmax = Tb

                    Dim ycalc = TrangeL.Select(Function(x) comp.GetLiquidDensity(x)).ToList()

                    comp.Comments += vbCrLf
                    comp.Comments += "Liquid Density regression residual = " + r_fit.ToString(ci) + vbCrLf
                    comp.Comments += "Regressed Data Table" + vbCrLf
                    comp.Comments += GetTable(TrangeL, DensL, ycalc, "T (K)", "rhoL (kg/m3)")

                    comp.Liquid_Density_Regression_Fit = r_fit
                    comp.Liquid_Density_Tabular_Data.XData = TrangeL
                    comp.Liquid_Density_Tabular_Data.YData = DensL
                    comp.Liquid_Density_Tabular_Data.XName = "Temperature"
                    comp.Liquid_Density_Tabular_Data.YName = "Density"
                    comp.Liquid_Density_Tabular_Data.XUnit = "K"
                    comp.Liquid_Density_Tabular_Data.YUnit = "kg/m3"

                Else

                    comp.Comments += vbCrLf
                    comp.Comments += "Liquid Density data unavailable, will be estimated." + vbCrLf

                End If

            Catch ex As Exception

                comp.Comments += vbCrLf
                comp.Comments += "Liquid Density data unavailable, will be estimated." + vbCrLf

            End Try

            'liquid viscosity

            Try

                instance = Py.Import("thermo.viscosity")
                calculator = instance.ViscosityLiquid(CASRN:=CAS, extrapolation:="interp1d")

                For Each item In TrangeL
                    Dim p = calculator.TP_or_T_dependent_property(item.ToPython(), 101325.0F.ToPython())
                    If p IsNot Nothing Then
                        value = p.ToString().ToDoubleFromInvariant()
                        ViscL.Add(value)
                    End If
                Next

                If Not ViscL.Sum = 0.0# Then

                    comp.LiquidViscosityEquation = 101

                    coeffs(0) = -17.255
                    coeffs(1) = 1576
                    coeffs(2) = 0.86191
                    coeffs(3) = 0
                    coeffs(4) = 0

                    obj = lmfit.GetCoeffs(TrangeL.ToArray, ViscL.ToArray, coeffs.Clone, ChEDLThermoLink.LMFit.FitType.LiqVisc, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                    fitcoeffs = obj(0)
                    r_fit = obj(2)
                    n_fit = obj(3)

                    comp.Liquid_Viscosity_Const_A = fitcoeffs(0)
                    comp.Liquid_Viscosity_Const_B = fitcoeffs(1)
                    comp.Liquid_Viscosity_Const_C = fitcoeffs(2)
                    comp.Liquid_Viscosity_Const_D = fitcoeffs(3)
                    comp.Liquid_Viscosity_Const_E = fitcoeffs(4)

                    Dim ycalc = TrangeL.Select(Function(x) comp.GetLiquidViscosity(x)).ToList()

                    comp.Comments += vbCrLf
                    comp.Comments += "Liquid Viscosity regression residual = " + r_fit.ToString(ci) + vbCrLf
                    comp.Comments += "Regressed Data Table" + vbCrLf
                    comp.Comments += GetTable(TrangeL, ViscL, ycalc, "T (K)", "muL (Pa.s)")

                    comp.Liquid_Viscosity_Regression_Fit = r_fit
                    comp.Liquid_Viscosity_Tabular_Data.XData = TrangeL
                    comp.Liquid_Viscosity_Tabular_Data.YData = ViscL
                    comp.Liquid_Viscosity_Tabular_Data.XName = "Temperature"
                    comp.Liquid_Viscosity_Tabular_Data.YName = "Viscosity"
                    comp.Liquid_Viscosity_Tabular_Data.XUnit = "K"
                    comp.Liquid_Viscosity_Tabular_Data.YUnit = "Pa.s"

                Else

                    comp.Comments += vbCrLf
                    comp.Comments += "Liquid Viscosity data unavailable, will be estimated." + vbCrLf

                End If

            Catch ex As Exception

                comp.Comments += vbCrLf
                comp.Comments += "Liquid Viscosity data unavailable, will be estimated." + vbCrLf

            End Try

            'vapor viscosity

            Try

                calculator = instance.ViscosityGas(CASRN:=CAS, extrapolation:="interp1d")

                For Each item In TrangeV
                    Dim p = calculator.TP_or_T_dependent_property(item.ToPython(), 101325.0F.ToPython())
                    If p IsNot Nothing Then
                        value = p.ToString().ToDoubleFromInvariant()
                        ViscV.Add(value)
                    End If
                Next

                If Not ViscV.Sum = 0.0# Then

                    comp.VaporViscosityEquation = 5

                    coeffs(0) = 0.0001
                    coeffs(1) = 0.0000001
                    coeffs(2) = 0.0#
                    coeffs(3) = 0.0#
                    coeffs(4) = 0.0#

                    obj = lmfit.GetCoeffs(TrangeV.ToArray, ViscV.ToArray, coeffs.Clone, ChEDLThermoLink.LMFit.FitType.Cp, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                    fitcoeffs = obj(0)
                    r_fit = obj(2)
                    n_fit = obj(3)

                    comp.Vapor_Viscosity_Const_A = fitcoeffs(0)
                    comp.Vapor_Viscosity_Const_B = fitcoeffs(1)
                    comp.Vapor_Viscosity_Const_C = fitcoeffs(2)
                    comp.Vapor_Viscosity_Const_D = fitcoeffs(3)
                    comp.Vapor_Viscosity_Const_E = fitcoeffs(4)

                    comp.Vapor_Viscosity_Tmin = Tb
                    comp.Vapor_Viscosity_Tmax = Tmax

                    Dim ycalc = TrangeV.Select(Function(x) comp.GetVaporViscosity(x)).ToList()

                    comp.Comments += vbCrLf
                    comp.Comments += "Vapor Viscosity regression residual = " + r_fit.ToString(ci) + vbCrLf
                    comp.Comments += "Regressed Data Table" + vbCrLf
                    comp.Comments += GetTable(TrangeV, ViscV, ycalc, "T (K)", "muV (Pa.s)")

                    comp.Vapor_Viscosity_Regression_Fit = r_fit
                    comp.Vapor_Viscosity_Tabular_Data.XData = TrangeV
                    comp.Vapor_Viscosity_Tabular_Data.YData = ViscV
                    comp.Vapor_Viscosity_Tabular_Data.XName = "Temperature"
                    comp.Vapor_Viscosity_Tabular_Data.YName = "Viscosity"
                    comp.Vapor_Viscosity_Tabular_Data.XUnit = "K"
                    comp.Vapor_Viscosity_Tabular_Data.YUnit = "Pa.s"

                Else

                    comp.Comments += vbCrLf
                    comp.Comments += "Vapor Viscosity data unavailable, will be estimated." + vbCrLf

                End If

            Catch ex As Exception

                comp.Comments += vbCrLf
                comp.Comments += "Vapor Viscosity data unavailable, will be estimated." + vbCrLf

            End Try

            'liquid thermal conductivity

            Try

                instance = Py.Import("thermo.thermal_conductivity")
                calculator = instance.ThermalConductivityLiquid(CASRN:=CAS, extrapolation:="interp1d")

                For Each item In TrangeL
                    Dim p = calculator.TP_or_T_dependent_property(item.ToPython(), 101325.0F.ToPython())
                    If p IsNot Nothing Then
                        value = p.ToString().ToDoubleFromInvariant()
                        TCL.Add(value)
                    End If
                Next

                If Not TCL.Sum = 0.0# Then

                    comp.LiquidThermalConductivityEquation = 5

                    coeffs(0) = 10
                    coeffs(1) = 0.001
                    coeffs(2) = 0.0#
                    coeffs(3) = 0.0#
                    coeffs(4) = 0.0#

                    obj = lmfit.GetCoeffs(TrangeL.ToArray, TCL.ToArray, coeffs.Clone, ChEDLThermoLink.LMFit.FitType.Cp, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                    fitcoeffs = obj(0)
                    r_fit = obj(2)
                    n_fit = obj(3)

                    comp.Liquid_Thermal_Conductivity_Const_A = fitcoeffs(0)
                    comp.Liquid_Thermal_Conductivity_Const_B = fitcoeffs(1)
                    comp.Liquid_Thermal_Conductivity_Const_C = fitcoeffs(2)
                    comp.Liquid_Thermal_Conductivity_Const_D = fitcoeffs(3)
                    comp.Liquid_Thermal_Conductivity_Const_E = fitcoeffs(4)

                    Dim ycalc = TrangeL.Select(Function(x) comp.GetLiquidThermalConductivity(x)).ToList()

                    comp.Comments += vbCrLf
                    comp.Comments += "Liquid Thermal Conductivity regression residual = " + r_fit.ToString(ci) + vbCrLf
                    comp.Comments += "Regressed Data Table" + vbCrLf
                    comp.Comments += GetTable(TrangeL, TCL, ycalc, "T (K)", "TCL (W/m.K)")

                    comp.Liquid_Thermal_Conductivity_Regression_Fit = r_fit
                    comp.Liquid_Thermal_Conductivity_Tabular_Data.XData = TrangeL
                    comp.Liquid_Thermal_Conductivity_Tabular_Data.YData = TCL
                    comp.Liquid_Thermal_Conductivity_Tabular_Data.XName = "Temperature"
                    comp.Liquid_Thermal_Conductivity_Tabular_Data.YName = "Thermal Conductivity"
                    comp.Liquid_Thermal_Conductivity_Tabular_Data.XUnit = "K"
                    comp.Liquid_Thermal_Conductivity_Tabular_Data.YUnit = "W/m.K"

                Else

                    comp.Comments += vbCrLf
                    comp.Comments += "Liquid Thermal Conductivity data unavailable, will be estimated." + vbCrLf

                End If

            Catch ex As Exception

                comp.Comments += vbCrLf
                comp.Comments += "Liquid Thermal Conductivity data unavailable, will be estimated." + vbCrLf

            End Try

            'vapor thermal conductivity

            Try

                calculator = instance.ThermalConductivityGas(CASRN:=CAS, extrapolation:="interp1d")

                For Each item In TrangeV
                    Dim p = calculator.TP_or_T_dependent_property(item.ToPython(), 101325.0F.ToPython())
                    If p IsNot Nothing Then
                        value = p.ToString().ToDoubleFromInvariant()
                        TCV.Add(value)
                    End If
                Next

                If Not TCV.Sum = 0.0# Then

                    comp.VaporThermalConductivityEquation = 5

                    coeffs(0) = 10
                    coeffs(1) = 0.001
                    coeffs(2) = 0.0#
                    coeffs(3) = 0.0#
                    coeffs(4) = 0.0#

                    obj = lmfit.GetCoeffs(TrangeV.ToArray, TCV.ToArray, coeffs.Clone, ChEDLThermoLink.LMFit.FitType.Cp, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                    fitcoeffs = obj(0)
                    r_fit = obj(2)
                    n_fit = obj(3)

                    comp.Vapor_Thermal_Conductivity_Const_A = fitcoeffs(0)
                    comp.Vapor_Thermal_Conductivity_Const_B = fitcoeffs(1)
                    comp.Vapor_Thermal_Conductivity_Const_C = fitcoeffs(2)
                    comp.Vapor_Thermal_Conductivity_Const_D = fitcoeffs(3)
                    comp.Vapor_Thermal_Conductivity_Const_E = fitcoeffs(4)

                    Dim ycalc = TrangeV.Select(Function(x) comp.GetVaporThermalConductivity(x)).ToList()

                    comp.Comments += vbCrLf
                    comp.Comments += "Vapor Thermal Conductivity regression residual = " + r_fit.ToString(ci) + vbCrLf
                    comp.Comments += "Regressed Data Table" + vbCrLf
                    comp.Comments += GetTable(TrangeV, TCV, ycalc, "T (K)", "TCV (W/m.K)")

                    comp.Vapor_Thermal_Conductivity_Regression_Fit = r_fit
                    comp.Vapor_Thermal_Conductivity_Tabular_Data.XData = TrangeV
                    comp.Vapor_Thermal_Conductivity_Tabular_Data.YData = TCV
                    comp.Vapor_Thermal_Conductivity_Tabular_Data.XName = "Temperature"
                    comp.Vapor_Thermal_Conductivity_Tabular_Data.YName = "Thermal Conductivity"
                    comp.Vapor_Thermal_Conductivity_Tabular_Data.XUnit = "K"
                    comp.Vapor_Thermal_Conductivity_Tabular_Data.YUnit = "W/m.K"

                Else

                    comp.Comments += vbCrLf
                    comp.Comments += "Vapor Thermal Conductivity data unavailable, will be estimated." + vbCrLf

                End If

            Catch ex As Exception

                comp.Comments += vbCrLf
                comp.Comments += "Vapor Thermal Conductivity data unavailable, will be estimated." + vbCrLf

            End Try

            'enthalpy of vaporization

            Try

                instance = Py.Import("thermo.phase_change")
                calculator = instance.EnthalpyVaporization(CASRN:=CAS, extrapolation:="interp1d")

                For Each item In TrangeL
                    Dim p = calculator.T_dependent_property(item.ToPython())
                    If p IsNot Nothing Then
                        value = p.ToString().ToDoubleFromInvariant()
                        Hvap.Add(value / comp.Molar_Weight)
                    End If
                Next

                If Not Hvap.Sum = 0.0# Then

                    comp.VaporizationEnthalpyEquation = 5

                    coeffs(0) = 300
                    coeffs(1) = 0.001
                    coeffs(2) = 0.0#
                    coeffs(3) = 0.0#
                    coeffs(4) = 0.0#

                    obj = lmfit.GetCoeffs(TrangeL.ToArray, Hvap.ToArray, coeffs.Clone, ChEDLThermoLink.LMFit.FitType.Cp, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                    fitcoeffs = obj(0)
                    r_fit = obj(2)
                    n_fit = obj(3)

                    comp.HVap_A = fitcoeffs(0)
                    comp.HVap_B = fitcoeffs(1)
                    comp.HVap_C = fitcoeffs(2)
                    comp.HVap_D = fitcoeffs(3)
                    comp.HVap_E = fitcoeffs(4)

                    Dim ycalc = TrangeL.Select(Function(x) comp.GetEnthalpyOfVaporization(x)).ToList()

                    comp.Comments += vbCrLf
                    comp.Comments += "Enthalpy of Vaporization regression residual = " + r_fit.ToString(ci) + vbCrLf
                    comp.Comments += "Regressed Data Table" + vbCrLf
                    comp.Comments += GetTable(TrangeL, Hvap, ycalc, "T (K)", "Hvap (kJ/kg.K)")

                    comp.Enthalpy_Of_Vaporization_Regression_Fit = r_fit
                    comp.Enthalpy_Of_Vaporization_Tabular_Data.XData = TrangeL
                    comp.Enthalpy_Of_Vaporization_Tabular_Data.YData = Hvap
                    comp.Enthalpy_Of_Vaporization_Tabular_Data.XName = "Temperature"
                    comp.Enthalpy_Of_Vaporization_Tabular_Data.YName = "Enthalpy"
                    comp.Enthalpy_Of_Vaporization_Tabular_Data.XUnit = "K"
                    comp.Enthalpy_Of_Vaporization_Tabular_Data.YUnit = "kJ/[kg.K]"

                Else

                    comp.Comments += vbCrLf
                    comp.Comments += "Enthalpy of Vaporization data unavailable, will be estimated." + vbCrLf

                End If

            Catch ex As Exception

                comp.Comments += vbCrLf
                comp.Comments += "Enthalpy of Vaporization data unavailable, will be estimated." + vbCrLf

            End Try

            'surface tension

            Try

                instance = Py.Import("thermo.interface")
                calculator = instance.SurfaceTension(CASRN:=CAS, extrapolation:="interp1d")

                For Each item In TrangeL
                    Dim p = calculator.T_dependent_property(item.ToPython())
                    If p IsNot Nothing Then
                        value = p.ToString().ToDoubleFromInvariant()
                        SurfT.Add(value)
                    End If
                Next

                If Not SurfT.Sum = 0.0# Then

                    comp.SurfaceTensionEquation = 5

                    coeffs(0) = 30
                    coeffs(1) = 0.001
                    coeffs(2) = 0.0#
                    coeffs(3) = 0.0#
                    coeffs(4) = 0.0#

                    obj = lmfit.GetCoeffs(TrangeL.ToArray, SurfT.ToArray, coeffs.Clone, ChEDLThermoLink.LMFit.FitType.Cp, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                    fitcoeffs = obj(0)
                    r_fit = obj(2)
                    n_fit = obj(3)

                    comp.Surface_Tension_Const_A = fitcoeffs(0)
                    comp.Surface_Tension_Const_B = fitcoeffs(1)
                    comp.Surface_Tension_Const_C = fitcoeffs(2)
                    comp.Surface_Tension_Const_D = fitcoeffs(3)
                    comp.Surface_Tension_Const_E = fitcoeffs(4)

                    Dim ycalc = TrangeL.Select(Function(x) comp.GetLiquidSurfaceTension(x)).ToList()

                    comp.Comments += vbCrLf
                    comp.Comments += "Surface Tension regression residual = " + r_fit.ToString(ci) + vbCrLf
                    comp.Comments += "Regressed Data Table" + vbCrLf
                    comp.Comments += GetTable(TrangeL, SurfT, ycalc, "T (K)", "sigma (N/m)")

                    comp.Surface_Tension_Regression_Fit = r_fit
                    comp.Surface_Tension_Tabular_Data.XData = TrangeL
                    comp.Surface_Tension_Tabular_Data.YData = SurfT
                    comp.Surface_Tension_Tabular_Data.XName = "Temperature"
                    comp.Surface_Tension_Tabular_Data.YName = "Surface Tension"
                    comp.Surface_Tension_Tabular_Data.XUnit = "K"
                    comp.Surface_Tension_Tabular_Data.YUnit = "N/m"

                Else

                    comp.Comments += vbCrLf
                    comp.Comments += "Surface Tension data unavailable, will be estimated." + vbCrLf

                End If

            Catch ex As Exception

                comp.Comments += vbCrLf
                comp.Comments += "Surface Tension data unavailable, will be estimated." + vbCrLf

            End Try

            Return comp

        End Using

    End Function

    Private Shared Function GetTable(x As List(Of Double), y As List(Of Double), ycalc As List(Of Double), xlabel As String, ylabel As String) As String

        Dim ci As System.Globalization.CultureInfo = New Globalization.CultureInfo("en-US")

        Dim sb As New System.Text.StringBuilder()

        sb.AppendLine(xlabel.PadRight(25) + (ylabel + " (exp)").PadRight(25) + (ylabel + " (calc)").PadRight(25) + "err (%)")
        For i = 0 To x.Count - 1
            Dim err1 = (y(i) - ycalc(i)) / y(i) * 100.0
            sb.AppendLine(x(i).ToString(ci).PadRight(25) + y(i).ToString(ci).PadRight(25) + ycalc(i).ToString(ci).PadRight(25) + err1.ToString(ci))
        Next

        Return sb.ToString()

    End Function

End Class
