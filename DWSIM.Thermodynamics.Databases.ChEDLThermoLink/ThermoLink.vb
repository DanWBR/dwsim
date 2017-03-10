Imports System.Text
Imports DWSIM.Libraries.PythonLink

Public Class ChEDLThermoParser

    Shared Function GetPythonInstance(Optional ByVal ppath As String = "") As Python

        If ppath = "" Then
            Return New Python(Global.DWSIM.GlobalSettings.Settings.PythonPath, False)
        Else
            Return New Python(ppath, False)
        End If

    End Function

    Shared Function GetSupportedCompounds() As List(Of String)

        Dim python = GetPythonInstance()

        Dim command = "from thermo import *" + System.Environment.NewLine +
                        "valid_CASs = [] " + System.Environment.NewLine +
                        "for CAS in set(TRC_gas_data.index):" + System.Environment.NewLine +
                        "   try:" + System.Environment.NewLine +
                        "       c = Chemical(CAS)" + System.Environment.NewLine +
                        "       if c.Tc is not None and c.Pc is not None and c.omega is not None and c.Cpgm is not None  and c.Psat is not None:" + System.Environment.NewLine +
                        "           valid_CASs.append(c.synonyms[0])" + System.Environment.NewLine +
                        "   except:" + System.Environment.NewLine +
                        "       pass" + System.Environment.NewLine + ""


        python.ExecuteCommand(command, True)
        Dim results = python.ExecuteCommand("print valid_CASs", False).Replace(", ", "|").TrimStart("[").TrimEnd("]").Split("|")

        python.PythonProcess.Kill()
        python = Nothing

        Return New List(Of String)(results)

    End Function

    Shared Function SearchCompound(searchstring As String) As List(Of String)

        Dim python = GetPythonInstance()

        Dim command = "from thermo import *" + System.Environment.NewLine +
                        "valid_CASs = []" + System.Environment.NewLine +
                        "c = None" + System.Environment.NewLine +
                        "try:" + System.Environment.NewLine +
                        "  c = Chemical('" + searchstring + "')" + System.Environment.NewLine +
                        "  if c is not None and c.Tc is not None and c.Pc is not None and c.omega is not None and c.Cpgm is not None  and c.Psat is not None:" + System.Environment.NewLine +
                        "   valid_CASs.append(c.IUPAC_name)" + System.Environment.NewLine +
                        "except:" + System.Environment.NewLine +
                        " pass" + System.Environment.NewLine + ""

        python.ExecuteCommand(command, True)
        Dim results = python.ExecuteCommand("if len(valid_CASs) > 0: print c.synonyms" + System.Environment.NewLine + "", False).Replace(", ", "|").TrimStart("[").TrimEnd("]").Split("|")

        python.PythonProcess.Kill()
        python = Nothing

        Return New List(Of String)(results.Select(Function(x) System.Globalization.CultureInfo.CurrentCulture.TextInfo.ToTitleCase(x)))

    End Function

    Shared Function GetCompoundData(id As String) As BaseClasses.ConstantProperties

        Dim python = GetPythonInstance("C:\Users\ptc0\Downloads\python_thermo\python-2.7.13.amd64")

        Dim command = "from thermo import *" + System.Environment.NewLine +
              "c = Chemical('" + id + "')"

        python.ExecuteCommand(command, True)

        Dim ci As System.Globalization.CultureInfo = New Globalization.CultureInfo("en-US")

        Dim comp As New BaseClasses.ConstantProperties

        comp.Comments = "Data from ChEDL Thermo Python Library (https://github.com/CalebBell/thermo)"

        comp.OriginalDB = "ChEDL Thermo"

        comp.CurrentDB = "ChEDL Thermo"

        'string properties

        comp.ID = New Random().Next(700001, 799999)
        comp.Name = id
        comp.InChI = python.ExecuteCommand("print c.InChI", False)
        comp.SMILES = python.ExecuteCommand("print c.smiles", False)
        comp.Formula = python.ExecuteCommand("print c.formula", False)
        comp.CAS_Number = python.ExecuteCommand("print c.CAS", False)

        'elements

        Dim elements = python.ExecuteCommand("print c.atoms", False).Trim("{", "}").Replace(" ", "").Replace("'", "").Split(",")
        For Each item In elements
            comp.Elements.Add(item.Split(":")(0), item.Split(":")(1))
        Next

        'critical properties
        comp.Critical_Temperature = python.GetScalar("print c.Tc")
        comp.Critical_Pressure = python.GetScalar("print c.Pc")
        comp.Critical_Volume = python.GetScalar("print c.Vc") * 1000
        comp.Critical_Compressibility = python.GetScalar("print c.Zc")
        comp.Acentric_Factor = python.GetScalar("print c.omega")
        comp.Z_Rackett = comp.Critical_Compressibility

        'basic props
        comp.Molar_Weight = python.GetScalar("print c.MW")
        comp.Normal_Boiling_Point = python.GetScalar("print c.Tb")
        comp.NBP = comp.Normal_Boiling_Point
        comp.TemperatureOfFusion = python.GetScalar("print c.Tm")

        comp.EnthalpyOfFusionAtTf = python.GetScalar("print c.Hfus") / 1000
        comp.IG_Enthalpy_of_Formation_25C = python.GetScalar("print c.Hf") / comp.Molar_Weight

        comp.Dipole_Moment = python.GetScalar("print c.dipole")

        'temperature-dependent data setup

        Dim lmfit As New LMFit

        Dim TrangeS, TrangeL, TrangeV As New List(Of Double), Tmin, Tb, Tmax, Tx As Double
        Tmin = comp.TemperatureOfFusion
        Tb = comp.Normal_Boiling_Point
        Tmax = comp.Critical_Temperature

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

        For Each item In TrangeL
            Pvap.Add(python.GetScalar("print c.VaporPressure(" + item.ToString(ci) + ")"))
        Next

        comp.VaporPressureEquation = 101

        coeffs(0) = 25
        coeffs(1) = 2000
        coeffs(2) = -5.245
        coeffs(3) = 0.0#
        coeffs(4) = 0.0#

        obj = lmfit.GetCoeffs(TrangeL.ToArray, Pvap.ToArray, coeffs.Clone, ChEDLThermoLink.LMFit.FitType.Pvap, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
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

        comp.Comments += vbCrLf
        comp.Comments += "Vapor Pressure regression residual = " + r_fit.ToString(ci) + vbCrLf
        comp.Comments += "Regressed Data Table" + vbCrLf
        comp.Comments += GetTable(TrangeL, Pvap, "T (K)", "Pvap (Pa)")

        'ideal gas cp

        For Each item In TrangeV
            CpIG.Add(python.GetScalar("print c.HeatCapacityGas(" + item.ToString(ci) + ")") / comp.Molar_Weight)
        Next

        comp.IdealgasCpEquation = 5

        coeffs(0) = 33.7
        coeffs(1) = 0.249
        coeffs(2) = 0.000253
        coeffs(3) = -0.000000384
        coeffs(4) = 0.000000000129

        obj = lmfit.GetCoeffs(TrangeL.ToArray, CpIG.ToArray, coeffs.Clone, ChEDLThermoLink.LMFit.FitType.Cp, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
        fitcoeffs = obj(0)
        r_fit = obj(2)
        n_fit = obj(3)

        comp.Ideal_Gas_Heat_Capacity_Const_A = fitcoeffs(0)
        comp.Ideal_Gas_Heat_Capacity_Const_B = fitcoeffs(1)
        comp.Ideal_Gas_Heat_Capacity_Const_C = fitcoeffs(2)
        comp.Ideal_Gas_Heat_Capacity_Const_D = fitcoeffs(3)
        comp.Ideal_Gas_Heat_Capacity_Const_E = fitcoeffs(4)

        comp.Comments += vbCrLf
        comp.Comments += "Ideal Gas Heat Capacity regression residual = " + r_fit.ToString(ci) + vbCrLf
        comp.Comments += "Regressed Data Table" + vbCrLf
        comp.Comments += GetTable(TrangeV, CpIG, "T (K)", "Cp (kJ/kg.K)")

        'liquid cp

        For Each item In TrangeL
            CpL.Add(python.GetScalar("print c.HeatCapacityLiquid(" + item.ToString(ci) + ")") / comp.Molar_Weight)
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

            comp.Comments += vbCrLf
            comp.Comments += "Liquid Heat Capacity regression residual = " + r_fit.ToString(ci) + vbCrLf
            comp.Comments += "Regressed Data Table" + vbCrLf
            comp.Comments += GetTable(TrangeV, CpL, "T (K)", "Cp (kJ/kg.K)")

        Else

            comp.Comments += vbCrLf
            comp.Comments += "Liquid Heat Capacity data unavailable, will be estimated." + vbCrLf

        End If

        'solid cp

        For Each item In TrangeS
            CpS.Add(python.GetScalar("print c.HeatCapacitySolid(" + item.ToString(ci) + ")") / comp.Molar_Weight)
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

            comp.Comments += vbCrLf
            comp.Comments += "Solid Heat Capacity regression residual = " + r_fit.ToString(ci) + vbCrLf
            comp.Comments += "Regressed Data Table" + vbCrLf
            comp.Comments += GetTable(TrangeS, CpS, "T (K)", "Cp (kJ/kg.K)")

        Else

            comp.Comments += vbCrLf
            comp.Comments += "Solid Heat Capacity data unavailable, will be estimated." + vbCrLf

        End If

        'solid density

        For Each item In TrangeS
            DensS.Add(1 / python.GetScalar("print c.VolumeSolid(" + item.ToString(ci) + ")") / 1000 * comp.Molar_Weight)
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

            comp.Comments += vbCrLf
            comp.Comments += "Solid Density regression residual = " + r_fit.ToString(ci) + vbCrLf
            comp.Comments += "Regressed Data Table" + vbCrLf
            comp.Comments += GetTable(TrangeS, DensS, "T (K)", "rhoS (kg/m3)")

        Else

            comp.Comments += vbCrLf
            comp.Comments += "Solid Density data unavailable, will be estimated." + vbCrLf

        End If

        'liquid density

        For Each item In TrangeL
            DensL.Add(1 / python.GetScalar("print c.VolumeLiquid(" + item.ToString(ci) + ", 101325)") / 1000 * comp.Molar_Weight)
        Next

        If Not DensL.Sum = Double.PositiveInfinity Then

            comp.LiquidDensityEquation = 105

            coeffs(4) = 0.0#
            coeffs(3) = 1.0#
            coeffs(2) = 647.3
            coeffs(1) = 0.14056
            coeffs(0) = 1.0#

            obj = lmfit.GetCoeffs(TrangeL.ToArray, DensL.ToArray, coeffs.Clone, ChEDLThermoLink.LMFit.FitType.LiqDens, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
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

            comp.Comments += vbCrLf
            comp.Comments += "Liquid Density regression residual = " + r_fit.ToString(ci) + vbCrLf
            comp.Comments += "Regressed Data Table" + vbCrLf
            comp.Comments += GetTable(TrangeL, DensL, "T (K)", "rhoL (kg/m3)")

        Else

            comp.Comments += vbCrLf
            comp.Comments += "Liquid Density data unavailable, will be estimated." + vbCrLf

        End If

        'liquid viscosity

        For Each item In TrangeL
            ViscL.Add(python.GetScalar("print c.ViscosityLiquid(" + item.ToString(ci) + ", 101325)"))
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

            comp.Comments += vbCrLf
            comp.Comments += "Liquid Viscosity regression residual = " + r_fit.ToString(ci) + vbCrLf
            comp.Comments += "Regressed Data Table" + vbCrLf
            comp.Comments += GetTable(TrangeL, ViscL, "T (K)", "muL (Pa.s)")

        Else

            comp.Comments += vbCrLf
            comp.Comments += "Liquid Viscosity data unavailable, will be estimated." + vbCrLf

        End If

        'vapor viscosity

        For Each item In TrangeV
            ViscV.Add(python.GetScalar("print c.ViscosityGas(" + item.ToString(ci) + ", 101325)"))
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

            comp.Comments += vbCrLf
            comp.Comments += "Vapor Viscosity regression residual = " + r_fit.ToString(ci) + vbCrLf
            comp.Comments += "Regressed Data Table" + vbCrLf
            comp.Comments += GetTable(TrangeV, ViscV, "T (K)", "muV (Pa.s)")

        Else

            comp.Comments += vbCrLf
            comp.Comments += "Vapor Viscosity data unavailable, will be estimated." + vbCrLf

        End If

        'liquid thermal conductivity

        For Each item In TrangeL
            TCL.Add(python.GetScalar("print c.ThermalConductivityLiquid(" + item.ToString(ci) + ", 101325)"))
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

            comp.Comments += vbCrLf
            comp.Comments += "Liquid Thermal Conductivity regression residual = " + r_fit.ToString(ci) + vbCrLf
            comp.Comments += "Regressed Data Table" + vbCrLf
            comp.Comments += GetTable(TrangeL, TCL, "T (K)", "TCL (W/m.K)")

        Else

            comp.Comments += vbCrLf
            comp.Comments += "Liquid Thermal Conductivity data unavailable, will be estimated." + vbCrLf

        End If

        'vapor thermal conductivity

        For Each item In TrangeV
            TCV.Add(python.GetScalar("print c.ThermalConductivityGas(" + item.ToString(ci) + ", 101325)"))
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

            comp.Comments += vbCrLf
            comp.Comments += "Vapor Thermal Conductivity regression residual = " + r_fit.ToString(ci) + vbCrLf
            comp.Comments += "Regressed Data Table" + vbCrLf
            comp.Comments += GetTable(TrangeV, TCV, "T (K)", "TCV (W/m.K)")

        Else

            comp.Comments += vbCrLf
            comp.Comments += "Vapor Thermal Conductivity data unavailable, will be estimated." + vbCrLf

        End If

        'enthalpy of vaporization

        python.ExecuteCommand("import os", True)
        python.ExecuteCommand("clear = lambda: os.system('cls')", True)
        python.ExecuteCommand("clear()", True)

        For Each item In TrangeL
            Hvap.Add(python.GetScalar("print c.EnthalpyVaporization(" + item.ToString(ci) + ")") / comp.Molar_Weight)
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

            comp.Comments += vbCrLf
            comp.Comments += "Enthalpy of Vaporization regression residual = " + r_fit.ToString(ci) + vbCrLf
            comp.Comments += "Regressed Data Table" + vbCrLf
            comp.Comments += GetTable(TrangeL, Hvap, "T (K)", "Hvap (kJ/kg.K)")

        Else

            comp.Comments += vbCrLf
            comp.Comments += "Enthalpy of Vaporization data unavailable, will be estimated." + vbCrLf

        End If

        'surface tension

        For Each item In TrangeL
            SurfT.Add(python.GetScalar("print c.SurfaceTension(" + item.ToString(ci) + ")"))
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

            comp.Comments += vbCrLf
            comp.Comments += "Surface Tension regression residual = " + r_fit.ToString(ci) + vbCrLf
            comp.Comments += "Regressed Data Table" + vbCrLf
            comp.Comments += GetTable(TrangeL, SurfT, "T (K)", "sigma (N/m)")

        Else

            comp.Comments += vbCrLf
            comp.Comments += "Surface Tension data unavailable, will be estimated." + vbCrLf

        End If

        python.PythonProcess.Kill()
        python = Nothing

        Return comp

    End Function

    Private Shared Function GetTable(x As List(Of Double), y As List(Of Double), xlabel As String, ylabel As String) As String

        Dim ci As System.Globalization.CultureInfo = New Globalization.CultureInfo("en-US")

        Dim i As Integer = 0

        Dim sb As New System.Text.StringBuilder()

        sb.AppendLine(xlabel.PadRight(40) + ylabel)
        For i = 0 To x.Count - 1
            sb.AppendLine(x(i).ToString(ci).PadRight(40) + y(i).ToString(ci))
        Next

        Return sb.ToString

    End Function

End Class
