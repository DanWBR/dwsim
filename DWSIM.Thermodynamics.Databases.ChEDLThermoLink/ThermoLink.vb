Imports System.Text
Imports DWSIM.Libraries.PythonLink

Public Class ChEDLThermoParser

    Shared Function GetPythonInstance() As Python

        'Return New Python(Global.DWSIM.GlobalSettings.Settings.PythonPath, False)

        Return New Python("E:\Downloads\python_thermo\python-2.7.13.amd64", False)

        'python.ExecuteCommand("import sys", True)

        'python.ExecuteCommand("from thermo.chemical import Chemical", True)

        'python.ExecuteCommand("tol = Chemical('toluene')", True)

        'Dim results = python.GetVector("tol.Tm, tol.Tc, tol.Pc")


    End Function

    Shared Sub GetSupportedCompounds()

        Dim python = GetPythonInstance()

        Dim command = "from thermo import *" + System.Environment.NewLine +
                        "valid_CASs = [] " + System.Environment.NewLine +
                        "for CAS in set(TRC_gas_data.index):" + System.Environment.NewLine +
                        "   try:" + System.Environment.NewLine +
                        "       c = Chemical(CAS)" + System.Environment.NewLine +
                        "       if c.Tc is not None and c.Pc is not None and c.omega is not None and c.Cpgm is not None  and c.Psat is not None:" + System.Environment.NewLine +
                        "           valid_CASs.append(CAS)" + System.Environment.NewLine +
                        "   except:" + System.Environment.NewLine +
                        "       pass" + System.Environment.NewLine + ""

        
        python.ExecuteCommand(command, True)
        Dim results = python.ExecuteCommand("print len(valid_CASs)", False)

    End Sub


End Class
