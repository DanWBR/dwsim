'    Rigorous Columns (Distillation and Absorption) Solvers
'    Copyright 2008-2022 Daniel Wagner O. de Medeiros
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

Imports DWSIM.MathOps.MathEx
Imports DWSIM.MathOps.MathEx.Optimization
Imports DWSIM.SharedClasses
Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms
Imports DWSIM.UnitOperations.UnitOperations
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.SepOps
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.SepOps.SolvingMethods
Imports DWSIM.UnitOperations.UnitOperations.Column
Imports System.Math

Namespace UnitOperations.Auxiliary.SepOps.SolvingMethods

    <System.Serializable()> Public Class WangHenkeMethod2

        Inherits ColumnSolver

        Private _subcoolingdeltat As Double = 0.0

        Public Overrides ReadOnly Property Name As String
            Get
                Return "Modified Wang-Henke Solver"
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String
            Get
                Return "Modified Wang-Henke Bubble-Point (MBP) Solver"
            End Get
        End Property

        Public Function Solve(ByVal rc As Column, ByVal nc As Integer, ByVal ns As Integer, ByVal maxits As Integer,
                                ByVal tol As Double(), ByVal F As Double(), ByVal V As Double(),
                                ByVal Q As Double(), ByVal L As Double(),
                                ByVal VSS As Double(), ByVal LSS As Double(), ByVal Kval()() As Double,
                                ByVal x()() As Double, ByVal y()() As Double, ByVal z()() As Double,
                                ByVal fc()() As Double,
                                ByVal HF As Double(), ByVal T As Double(), ByVal P As Double(),
                                ByVal condt As DistillationColumn.condtype,
                                ByVal stopatitnumber As Integer,
                                ByVal eff() As Double,
                                ByVal coltype As Column.ColType,
                                ByVal pp As PropertyPackages.PropertyPackage,
                                ByVal specs As Dictionary(Of String, SepOps.ColumnSpec),
                                ByVal IdealK As Boolean, ByVal IdealH As Boolean) As Object

            Dim tolerance = tol(0)

            Dim flashalgs As New List(Of FlashAlgorithm)

            For ia As Integer = 0 To ns
                Dim flashcopy = pp.FlashBase.GetNewInstance()
                If flashcopy Is Nothing Then
                    flashalgs.Add(New NestedLoops With {.FlashSettings = pp.FlashBase.FlashSettings})
                Else
                    flashalgs.Add(flashcopy)
                End If
            Next

            Dim spval1, spval2 As Double
            Dim spci1, spci2 As Integer

            Select Case specs("C").SType
                Case ColumnSpec.SpecType.Component_Fraction,
                      ColumnSpec.SpecType.Stream_Ratio,
                      ColumnSpec.SpecType.Component_Recovery
                    spval1 = specs("C").SpecValue
                Case Else
                    spval1 = SystemsOfUnits.Converter.ConvertToSI(specs("C").SpecUnit, specs("C").SpecValue)
            End Select
            spci1 = specs("C").ComponentIndex
            Select Case specs("R").SType
                Case ColumnSpec.SpecType.Component_Fraction,
                      ColumnSpec.SpecType.Stream_Ratio,
                      ColumnSpec.SpecType.Component_Recovery
                    spval2 = specs("R").SpecValue
                Case Else
                    spval2 = SystemsOfUnits.Converter.ConvertToSI(specs("R").SpecUnit, specs("R").SpecValue)
            End Select
            spci2 = specs("R").ComponentIndex

            Dim specC_OK, specR_OK As Boolean

            Select Case specs("C").SType
                Case ColumnSpec.SpecType.Component_Fraction,
                ColumnSpec.SpecType.Component_Mass_Flow_Rate,
                ColumnSpec.SpecType.Component_Molar_Flow_Rate,
                ColumnSpec.SpecType.Component_Recovery,
                ColumnSpec.SpecType.Temperature
                    specC_OK = False
                Case Else
                    specC_OK = True
            End Select

            Select Case specs("R").SType
                Case ColumnSpec.SpecType.Component_Fraction,
                ColumnSpec.SpecType.Component_Mass_Flow_Rate,
                ColumnSpec.SpecType.Component_Molar_Flow_Rate,
                ColumnSpec.SpecType.Component_Recovery,
                ColumnSpec.SpecType.Temperature,
                ColumnSpec.SpecType.Stream_Ratio
                    specR_OK = False
                Case Else
                    specR_OK = True
            End Select

            If DirectCast(rc, DistillationColumn).ReboiledAbsorber Then specC_OK = True
            If DirectCast(rc, DistillationColumn).RefluxedAbsorber Then specR_OK = True

            Dim ObjFunctionValues As New List(Of Double)
            Dim ResultsVector As New List(Of Object)

            If Not specC_OK And Not specR_OK Then

                Dim refluxratio As Double = 0.0
                If specs("C").InitialEstimate.HasValue Then
                    refluxratio = specs("C").InitialEstimate.GetValueOrDefault()
                Else
                    If condt <> Column.condtype.Full_Reflux Then
                        refluxratio = (L(0) + LSS(0)) / LSS(0)
                    Else
                        refluxratio = (V(1) + F(0)) / V(0) - 1
                    End If
                End If

                Dim bottomsrate As Double = L.Last

                Dim newspecs As New Dictionary(Of String, ColumnSpec)
                Dim cspec As New ColumnSpec()
                cspec.SpecValue = refluxratio
                cspec.SType = ColumnSpec.SpecType.Stream_Ratio
                Dim rspec As New ColumnSpec()
                rspec.SpecValue = bottomsrate
                rspec.SType = ColumnSpec.SpecType.Product_Molar_Flow_Rate
                newspecs.Add("C", cspec)
                newspecs.Add("R", rspec)

                Dim result As Object = Nothing

                result = Solve_Internal(rc, nc, ns, maxits, tolerance, F, V, Q, L, VSS, LSS, Kval,
                                           x, y, z, fc, HF, T, P, condt, 50, eff,
                                           coltype, pp, newspecs, IdealK, IdealH, flashalgs)

                T = result(0)
                V = result(1)
                L = result(2)
                VSS = result(3)
                LSS = result(4)
                y = result(5)
                x = result(6)
                Kval = result(7)
                Q = result(8)

                bottomsrate = L.Last

                Dim counter As Integer = 0

                Dim errfunc1 As Double = 1.0E+20
                Dim errfunc2 As Double = 1.0E+20

                Dim fbody As Func(Of Double(), Integer, Double, Integer, Double()) =
                    Function(xvars, mode, tol1, runs)

                        cspec.SpecValue = xvars(0)
                        rspec.SpecValue = xvars(1)

                        If cspec.SpecValue < 0 Then
                            cspec.SpecValue = -xvars(0)
                        End If

                        If rspec.SpecValue < 0 Then
                            rspec.SpecValue = -xvars(1)
                        End If

                        result = Solve_Internal(rc, nc, ns, maxits, tol1, F, V, Q, L, VSS, LSS, Kval,
                                x, y, z, fc, HF, T, P, condt, runs, eff,
                                coltype, pp, newspecs, IdealK, IdealH, flashalgs)

                        'Return New Object() {Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ic, t_error}

                        errfunc1 = 0.0
                        errfunc2 = 0.0

                        Dim T2 = result(0)
                        Dim V2 = result(1)
                        Dim L2 = result(2)
                        Dim VSS2 = result(3)
                        Dim LSS2 = result(4)
                        Dim y2 = result(5)
                        Dim x2 = result(6)
                        Dim Kval2 = result(7)
                        Dim Q2 = result(8)

                        Select Case specs("C").SType
                            Case ColumnSpec.SpecType.Component_Fraction
                                If condt <> Column.condtype.Full_Reflux Then
                                    If specs("C").SpecUnit = "M" Or specs("C").SpecUnit = "Molar" Then
                                        errfunc1 = Log(x2(0)(spci1) / spval1)
                                        specs("C").CalculatedValue = x2(0)(spci1)
                                    Else 'W
                                        errfunc1 = Log((pp.AUX_CONVERT_MOL_TO_MASS(x2(0))(spci1)) / spval1)
                                        specs("C").CalculatedValue = pp.AUX_CONVERT_MOL_TO_MASS(x2(0))(spci1)
                                    End If
                                Else
                                    If specs("C").SpecUnit = "M" Or specs("C").SpecUnit = "Molar" Then
                                        errfunc1 = Log((y2(0)(spci1)) / spval1)
                                        specs("C").CalculatedValue = y2(0)(spci1)
                                    Else 'W
                                        errfunc1 = Log((pp.AUX_CONVERT_MOL_TO_MASS(y2(0))(spci1)) / spval1)
                                        specs("C").CalculatedValue = pp.AUX_CONVERT_MOL_TO_MASS(y2(0))(spci1)
                                    End If
                                End If
                            Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                                If condt <> Column.condtype.Full_Reflux Then
                                    errfunc1 = Log((LSS2(0) * x2(0)(spci1) * pp.RET_VMM()(spci1) / 1000) / spval1)
                                    specs("C").CalculatedValue = LSS2(0) * x2(0)(spci1) * pp.RET_VMM()(spci1) / 1000
                                Else
                                    errfunc1 = Log((V2(0) * y2(0)(spci1) * pp.RET_VMM()(spci1) / 1000) / spval1)
                                    specs("C").CalculatedValue = V2(0) * y2(0)(spci1) * pp.RET_VMM()(spci1) / 1000
                                End If
                            Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                                If condt <> Column.condtype.Full_Reflux Then
                                    errfunc1 = Log((LSS2(0) * x2(0)(spci1)) / spval1)
                                    specs("C").CalculatedValue = LSS2(0) * x2(0)(spci1)
                                Else
                                    errfunc1 = Log((V2(0) * y2(0)(spci1)) / spval1)
                                    specs("C").CalculatedValue = V2(0) * y2(0)(spci1)
                                End If
                            Case ColumnSpec.SpecType.Component_Recovery
                                Dim rec As Double = spval1 / 100
                                Dim sumc As Double = 0
                                For j = 0 To ns
                                    sumc += z(j)(spci1) * F(j)
                                Next
                                If condt <> Column.condtype.Full_Reflux Then
                                    errfunc1 = Log(LSS2(0) * x2(0)(spci1) / sumc / rec)
                                    specs("C").CalculatedValue = LSS2(0) * x2(0)(spci1) / sumc * 100
                                Else
                                    errfunc1 = Log(V2(0) * y2(0)(spci1) / sumc / rec)
                                    specs("C").CalculatedValue = V2(0) * y2(0)(spci1) / sumc * 100
                                End If
                            Case ColumnSpec.SpecType.Temperature
                                errfunc1 = Log((T2(0)) / spval1)
                                specs("C").CalculatedValue = T2(0)
                        End Select

                        Select Case specs("R").SType
                            Case ColumnSpec.SpecType.Component_Fraction
                                If specs("R").SpecUnit = "M" Or specs("R").SpecUnit = "Molar" Then
                                    errfunc2 = Log((x2(ns)(spci2)) / spval2)
                                    specs("R").CalculatedValue = x2(ns)(spci1)
                                Else 'W
                                    errfunc2 = Log((pp.AUX_CONVERT_MOL_TO_MASS(x2(ns))(spci2)) / spval2)
                                    specs("R").CalculatedValue = pp.AUX_CONVERT_MOL_TO_MASS(x2(ns))(spci2)
                                End If
                            Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                                errfunc2 = Log((L2(ns) * x2(ns)(spci2) * pp.RET_VMM()(spci2) / 1000) / spval2)
                                specs("R").CalculatedValue = L2(ns) * x2(ns)(spci2) * pp.RET_VMM()(spci2) / 1000
                            Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                                errfunc2 = Log((L2(ns) * x2(ns)(spci2)) / spval2)
                                specs("R").CalculatedValue = L2(ns) * x2(ns)(spci2)
                            Case ColumnSpec.SpecType.Component_Recovery
                                Dim rec As Double = spval2 / 100
                                Dim sumc As Double = 0
                                For j = 0 To ns
                                    sumc += z(j)(spci2) * F(j)
                                Next
                                errfunc2 = Log(L2(ns) * x2(ns)(spci2) / sumc / rec)
                                specs("R").CalculatedValue = L2(ns) * x2(ns)(spci2) / sumc * 100
                            Case ColumnSpec.SpecType.Temperature
                                errfunc2 = Log(T2(ns) / spval2)
                                specs("R").CalculatedValue = T2(ns)
                            Case ColumnSpec.SpecType.Stream_Ratio
                                errfunc2 = Log(V2(ns) / L2(ns) / spval2)
                                specs("R").CalculatedValue = V2(ns) / L2(ns)
                        End Select

                        counter += 1

                        If Math.IEEERemainder(counter, 10) = 0.0 Then
                            pp.Flowsheet?.ShowMessage(String.Format(rc.GraphicObject.Tag + ": [BP Solver] external iteration #{0}, current objective function (error) value = {1}", counter, (errfunc1 + errfunc2) ^ 2), IFlowsheet.MessageType.Information)
                        End If

                        ResultsVector.Add(result)
                        ObjFunctionValues.Add(errfunc1 ^ 2 + errfunc2 ^ 2)

                        Return New Double() {errfunc1, errfunc2}

                    End Function

                If Double.IsNaN(errfunc1 + errfunc2) Then
                    Throw New Exception(pp.Flowsheet?.GetTranslatedString("DCGeneralError"))
                End If

                Dim bestset = MathNet.Numerics.RootFinding.Broyden.FindRoot(Function(xv0)
                                                                                Return fbody.Invoke(xv0, 0, tolerance, stopatitnumber)
                                                                            End Function,
                                                                       New Double() {refluxratio, bottomsrate}, tolerance, maxits)

                'Dim newton As New NewtonSolver
                'newton.Tolerance = tolerance
                'newton.MaxIterations = maxits
                'newton.Solve(Function(xv0)
                '                 Return fbody.Invoke(xv0, 0, tolerance, stopatitnumber)
                '             End Function, New Double() {refluxratio, bottomsrate})

                result = ResultsVector(ObjFunctionValues.IndexOf(ObjFunctionValues.Min))

                pp.Flowsheet?.ShowMessage(String.Format(rc.GraphicObject.Tag + ": [BP Solver] converged at external iteration #{0}, final objective function (error) value = {1}", counter, (errfunc1 + errfunc2) ^ 2), IFlowsheet.MessageType.Information)

                Return result

            ElseIf Not specC_OK Then

                Dim refluxratio As Double = 0.0
                If specs("C").InitialEstimate.HasValue Then
                    refluxratio = specs("C").InitialEstimate.GetValueOrDefault()
                Else
                    If condt <> Column.condtype.Full_Reflux Then
                        refluxratio = (L(0) + LSS(0)) / LSS(0)
                    Else
                        refluxratio = (V(1) + F(0)) / V(0) - 1
                    End If
                End If

                Dim newspecs As New Dictionary(Of String, ColumnSpec)
                Dim cspec As New ColumnSpec()
                cspec.SpecValue = refluxratio
                cspec.SType = ColumnSpec.SpecType.Stream_Ratio

                newspecs.Add("C", cspec)
                newspecs.Add("R", specs("R"))

                Dim result As Object = Nothing

                result = Solve_Internal(rc, nc, ns, maxits, tolerance, F, V, Q, L, VSS, LSS, Kval,
                                       x, y, z, fc, HF, T, P, condt, stopatitnumber, eff,
                                       coltype, pp, newspecs, IdealK, IdealH, flashalgs)

                T = result(0)
                V = result(1)
                L = result(2)
                VSS = result(3)
                LSS = result(4)
                y = result(5)
                x = result(6)
                Kval = result(7)
                Q = result(8)

                Dim counter As Integer = 0

                Dim errfunc As Double = 1.0E+20

                MathNet.Numerics.RootFinding.Broyden.FindRoot(
                    Function(xvars)

                        cspec.SpecValue = xvars(0)

                        If cspec.SpecValue < 0 Then cspec.SpecValue = -cspec.SpecValue

                        result = Solve_Internal(rc, nc, ns, maxits, tolerance, F, V, Q, L, VSS, LSS, Kval,
                                            x, y, z, fc, HF, T, P, condt, stopatitnumber, eff,
                                            coltype, pp, newspecs, IdealK, IdealH, flashalgs)

                        errfunc = 0.0

                        Dim T2 = result(0)
                        Dim V2 = result(1)
                        Dim L2 = result(2)
                        Dim VSS2 = result(3)
                        Dim LSS2 = result(4)
                        Dim y2 = result(5)
                        Dim x2 = result(6)
                        Dim Kval2 = result(7)
                        Dim Q2 = result(8)

                        Select Case specs("C").SType
                            Case ColumnSpec.SpecType.Component_Fraction
                                If condt <> Column.condtype.Full_Reflux Then
                                    If specs("C").SpecUnit = "M" Or specs("C").SpecUnit = "Molar" Then
                                        errfunc = Log(x2(0)(spci1) / spval1)
                                        specs("C").CalculatedValue = x2(0)(spci1)
                                    Else 'W
                                        errfunc = Log((pp.AUX_CONVERT_MOL_TO_MASS(x2(0))(spci1)) / spval1)
                                        specs("C").CalculatedValue = pp.AUX_CONVERT_MOL_TO_MASS(x2(0))(spci1)
                                    End If
                                Else
                                    If specs("C").SpecUnit = "M" Or specs("C").SpecUnit = "Molar" Then
                                        errfunc = Log((y2(0)(spci1)) / spval1)
                                        specs("C").CalculatedValue = y2(0)(spci1)
                                    Else 'W
                                        errfunc = Log((pp.AUX_CONVERT_MOL_TO_MASS(y2(0))(spci1)) / spval1)
                                        specs("C").CalculatedValue = pp.AUX_CONVERT_MOL_TO_MASS(y2(0))(spci1)
                                    End If
                                End If
                            Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                                If condt <> Column.condtype.Full_Reflux Then
                                    errfunc = Log((LSS2(0) * x2(0)(spci1) * pp.RET_VMM()(spci1) / 1000) / spval1)
                                    specs("C").CalculatedValue = LSS2(0) * x2(0)(spci1) * pp.RET_VMM()(spci1) / 1000
                                Else
                                    errfunc = Log((V2(0) * y2(0)(spci1) * pp.RET_VMM()(spci1) / 1000) / spval1)
                                    specs("C").CalculatedValue = V2(0) * y2(0)(spci1) * pp.RET_VMM()(spci1) / 1000
                                End If
                            Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                                If condt <> Column.condtype.Full_Reflux Then
                                    errfunc = Log((LSS2(0) * x2(0)(spci1)) / spval1)
                                    specs("C").CalculatedValue = LSS2(0) * x2(0)(spci1)
                                Else
                                    errfunc = Log((V2(0) * y2(0)(spci1)) / spval1)
                                    specs("C").CalculatedValue = V2(0) * y2(0)(spci1)
                                End If
                            Case ColumnSpec.SpecType.Component_Recovery
                                Dim rec As Double = spval1 / 100
                                Dim sumc As Double = 0
                                For j = 0 To ns
                                    sumc += z(j)(spci1) * F(j)
                                Next
                                If condt <> Column.condtype.Full_Reflux Then
                                    errfunc = Log(LSS2(0) * x2(0)(spci1) / sumc / rec)
                                    specs("C").CalculatedValue = LSS2(0) * x2(0)(spci1) / sumc * 100
                                Else
                                    errfunc = Log(V2(0) * y2(0)(spci1) / sumc / rec)
                                    specs("C").CalculatedValue = V2(0) * y2(0)(spci1) / sumc * 100
                                End If
                            Case ColumnSpec.SpecType.Temperature
                                errfunc = Log((T2(0)) / spval1)
                                specs("C").CalculatedValue = T2(0)
                        End Select

                        Select Case specs("R").SType
                            Case ColumnSpec.SpecType.Component_Fraction
                                If specs("R").SpecUnit = "M" Or specs("R").SpecUnit = "Molar" Then
                                    specs("R").CalculatedValue = x2(ns)(spci1)
                                Else 'W
                                    specs("R").CalculatedValue = pp.AUX_CONVERT_MOL_TO_MASS(x2(ns))(spci2)
                                End If
                            Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                                specs("R").CalculatedValue = L2(ns) * x2(ns)(spci2) * pp.RET_VMM()(spci2) / 1000
                            Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                                specs("R").CalculatedValue = L2(ns) * x2(ns)(spci2)
                            Case ColumnSpec.SpecType.Component_Recovery
                                Dim sumc As Double = 0
                                For j = 0 To ns
                                    sumc += z(j)(spci2) * F(j)
                                Next
                                specs("R").CalculatedValue = L2(ns) * x2(ns)(spci2) / sumc * 100
                            Case ColumnSpec.SpecType.Temperature
                                specs("R").CalculatedValue = T2(ns)
                            Case ColumnSpec.SpecType.Stream_Ratio
                                specs("R").CalculatedValue = V2(ns) / L2(ns)
                        End Select

                        counter += 1

                        If Math.IEEERemainder(counter, 10) = 0.0 Then
                            pp.Flowsheet?.ShowMessage(String.Format(rc.GraphicObject.Tag + ": [BP Solver] external iteration #{0}, current objective function (error) value = {1}", counter, errfunc), IFlowsheet.MessageType.Information)
                        End If

                        ResultsVector.Add(result)
                        ObjFunctionValues.Add(errfunc ^ 2)

                        Return New Double() {errfunc}

                    End Function, New Double() {refluxratio}, tolerance, maxits)

                If Double.IsNaN(errfunc) Or errfunc > tolerance Then Throw New Exception(pp.Flowsheet?.GetTranslatedString("DCGeneralError"))

                result = ResultsVector(ObjFunctionValues.IndexOf(ObjFunctionValues.Min))

                pp.Flowsheet?.ShowMessage(String.Format(rc.GraphicObject.Tag + ": [BP Solver] converged at external iteration #{0}, final objective function (error) value = {1}", counter, errfunc ^ 2), IFlowsheet.MessageType.Information)

                Return result

            ElseIf Not specR_OK Then

                Dim bottomsrate As Double = L.Last

                Dim newspecs As New Dictionary(Of String, ColumnSpec)
                Dim rspec As New ColumnSpec()
                rspec.SpecValue = bottomsrate
                rspec.SType = ColumnSpec.SpecType.Product_Molar_Flow_Rate
                newspecs.Add("C", specs("C"))
                newspecs.Add("R", rspec)

                Dim result As Object = Nothing

                result = Solve_Internal(rc, nc, ns, maxits, tolerance, F, V, Q, L, VSS, LSS, Kval,
                                       x, y, z, fc, HF, T, P, condt, stopatitnumber, eff,
                                       coltype, pp, newspecs, IdealK, IdealH, flashalgs)

                T = result(0)
                V = result(1)
                L = result(2)
                VSS = result(3)
                LSS = result(4)
                y = result(5)
                x = result(6)
                Kval = result(7)
                Q = result(8)

                bottomsrate = L.Last

                Dim counter As Integer = 0
                Dim errfunc As Double = 1.0E+20

                Dim freb As Func(Of Double, Integer, Double) =
                Function(xvar, mode)

                    rspec.SpecValue = xvar

                    result = Solve_Internal(rc, nc, ns, maxits, tolerance, F, V, Q, L, VSS, LSS, Kval,
                                                x, y, z, fc, HF, T, P, condt, stopatitnumber, eff,
                                                coltype, pp, newspecs, IdealK, IdealH, flashalgs)

                    errfunc = 0.0

                    Dim T2 = result(0)
                    Dim V2 = result(1)
                    Dim L2 = result(2)
                    Dim VSS2 = result(3)
                    Dim LSS2 = result(4)
                    Dim y2 = result(5)
                    Dim x2 = result(6)
                    Dim Kval2 = result(7)
                    Dim Q2 = result(8)

                    If mode = 1 Then
                        T = result(0)
                        V = result(1)
                        L = result(2)
                        VSS = result(3)
                        LSS = result(4)
                        y = result(5)
                        x = result(6)
                        Kval = result(7)
                        Q = result(8)
                    End If

                    Select Case specs("C").SType
                        Case ColumnSpec.SpecType.Component_Fraction
                            If condt <> Column.condtype.Full_Reflux Then
                                If specs("C").SpecUnit = "M" Or specs("C").SpecUnit = "Molar" Then
                                    specs("C").CalculatedValue = x2(0)(spci1)
                                Else 'W
                                    specs("C").CalculatedValue = pp.AUX_CONVERT_MOL_TO_MASS(x2(0))(spci1)
                                End If
                            Else
                                If specs("C").SpecUnit = "M" Or specs("C").SpecUnit = "Molar" Then
                                    specs("C").CalculatedValue = y2(0)(spci1)
                                Else 'W
                                    specs("C").CalculatedValue = pp.AUX_CONVERT_MOL_TO_MASS(y2(0))(spci1)
                                End If
                            End If
                        Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                            If condt <> Column.condtype.Full_Reflux Then
                                specs("C").CalculatedValue = LSS2(0) * x2(0)(spci1) * pp.RET_VMM()(spci1) / 1000
                            Else
                                specs("C").CalculatedValue = V2(0) * y2(0)(spci1) * pp.RET_VMM()(spci1) / 1000
                            End If
                        Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                            If condt <> Column.condtype.Full_Reflux Then
                                specs("C").CalculatedValue = LSS2(0) * x2(0)(spci1)
                            Else
                                specs("C").CalculatedValue = V2(0) * y2(0)(spci1)
                            End If
                        Case ColumnSpec.SpecType.Component_Recovery
                            Dim sumc As Double = 0
                            For j = 0 To ns
                                sumc += z(j)(spci1) * F(j)
                            Next
                            If condt <> Column.condtype.Full_Reflux Then
                                specs("C").CalculatedValue = LSS2(0) * x2(0)(spci1) / sumc * 100
                            Else
                                specs("C").CalculatedValue = V2(0) * y2(0)(spci1) / sumc * 100
                            End If
                        Case ColumnSpec.SpecType.Temperature
                            specs("C").CalculatedValue = T2(0)
                    End Select

                    Select Case specs("R").SType
                        Case ColumnSpec.SpecType.Component_Fraction
                            If specs("R").SpecUnit = "M" Or specs("R").SpecUnit = "Molar" Then
                                errfunc = Log((x2(ns)(spci2)) / spval2)
                                specs("R").CalculatedValue = x2(ns)(spci1)
                            Else 'W
                                errfunc = Log((pp.AUX_CONVERT_MOL_TO_MASS(x2(ns))(spci2)) / spval2)
                                specs("R").CalculatedValue = pp.AUX_CONVERT_MOL_TO_MASS(x2(ns))(spci2)
                            End If
                        Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                            errfunc = Log((L2(ns) * x2(ns)(spci2) * pp.RET_VMM()(spci2) / 1000) / spval2)
                            specs("R").CalculatedValue = L2(ns) * x2(ns)(spci2) * pp.RET_VMM()(spci2) / 1000
                        Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                            errfunc = Log((L2(ns) * x2(ns)(spci2)) / spval2)
                            specs("R").CalculatedValue = L2(ns) * x2(ns)(spci2)
                        Case ColumnSpec.SpecType.Component_Recovery
                            Dim rec As Double = spval2 / 100
                            Dim sumc As Double = 0
                            For j = 0 To ns
                                sumc += z(j)(spci2) * F(j)
                            Next
                            errfunc = Log(L2(ns) * x2(ns)(spci2) / sumc / rec)
                            specs("R").CalculatedValue = L2(ns) * x2(ns)(spci2) / sumc * 100
                        Case ColumnSpec.SpecType.Temperature
                            errfunc = Log(T2(ns) / spval2)
                            specs("R").CalculatedValue = T2(ns)
                        Case ColumnSpec.SpecType.Stream_Ratio
                            errfunc = Log(V2(ns) / L2(ns) / spval2)
                            specs("R").CalculatedValue = V2(ns) / L2(ns)
                    End Select

                    counter += 1

                    If Math.IEEERemainder(counter, 10) = 0.0 Then
                        pp.Flowsheet?.ShowMessage(String.Format(rc.GraphicObject.Tag + ": [BP Solver] external iteration #{0}, current objective function (error) value = {1}", counter, errfunc), IFlowsheet.MessageType.Information)
                    End If

                    ResultsVector.Add(result)
                    ObjFunctionValues.Add(errfunc ^ 2)

                    Return errfunc

                End Function

                Dim success As Boolean = False

                counter = 0
                errfunc = 1.0E+20

                ObjFunctionValues.Clear()
                ResultsVector.Clear()

                Try
                    MathNet.Numerics.RootFinding.Broyden.FindRoot(Function(xvars)
                                                                      If xvars(0) > F.Sum Then
                                                                          xvars(0) = 0.99 * F.Sum
                                                                      End If
                                                                      Return New Double() {freb.Invoke(xvars(0), 0)}
                                                                  End Function, New Double() {bottomsrate}, tolerance, maxits)
                    success = True
                Catch ex As Exception
                    success = False
                End Try

                If Not success Then

                    counter = 0
                    errfunc = 1.0E+20

                    ObjFunctionValues.Clear()
                    ResultsVector.Clear()

                    Try
                        Dim br As New DWSIM.MathOps.MathEx.BrentOpt.Brent()
                        br.BrentOpt2(bottomsrate * 0.1, F.Sum, 30, tolerance, maxits, Function(xvar)
                                                                                          Return freb.Invoke(xvar, 1)
                                                                                      End Function)
                        success = True
                    Catch ex As Exception
                        success = False
                    End Try

                End If

                If Not success Then

                    counter = 0
                    errfunc = 1.0E+20

                    ObjFunctionValues.Clear()
                    ResultsVector.Clear()

                    MathNet.Numerics.RootFinding.Brent.FindRootExpand(Function(xvar)
                                                                          Return freb.Invoke(xvar, 0)
                                                                      End Function,
                                                                          bottomsrate * 0.5,
                                                                          bottomsrate * 1.02, tolerance, maxits)

                End If

                If Double.IsNaN(errfunc) Or Math.Abs(errfunc) > tolerance Then Throw New Exception(pp.Flowsheet?.GetTranslatedString("DCGeneralError"))

                result = ResultsVector(ObjFunctionValues.IndexOf(ObjFunctionValues.Min))

                pp.Flowsheet?.ShowMessage(String.Format(rc.GraphicObject.Tag + ": [BP Solver] converged at external iteration #{0}, final objective function (error) value = {1}", counter, errfunc ^ 2), IFlowsheet.MessageType.Information)

                Return result

            Else

                Return Solve_Internal(rc, nc, ns, maxits, tolerance, F, V, Q, L, VSS, LSS, Kval, x, y, z, fc, HF, T, P, condt,
                                      stopatitnumber, eff, coltype, pp, specs, IdealK, IdealH, flashalgs)

            End If

        End Function

        Public Function Solve_Internal(rc As Column, nc As Integer, ns As Integer, maxits As Integer,
                                 tolerance As Double, F As Double(), V As Double(),
                                 Q As Double(), L As Double(),
                                 VSS As Double(), LSS As Double(), Kval()() As Double,
                                 x()() As Double, y()() As Double, z()() As Double,
                                 fc()() As Double,
                                 HF As Double(), T As Double(), P As Double(),
                                 condt As DistillationColumn.condtype,
                                 stopatitnumber As Integer,
                                 eff() As Double,
                                 coltype As Column.ColType,
                                 pp As PropertyPackages.PropertyPackage,
                                 specs As Dictionary(Of String, SepOps.ColumnSpec),
                                 IdealK As Boolean, IdealH As Boolean,
                                 flashalgs As List(Of FlashAlgorithm)) As Object

            rc.ColumnSolverConvergenceReport = ""

            Dim reporter As Text.StringBuilder = Nothing

            If rc.CreateSolverConvergengeReport Then reporter = New Text.StringBuilder()

            pp.CurrentMaterialStream.Flowsheet.CheckStatus()

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Solve", "Bubble-Point (BP) Method", "Wang-Henke Bubble-Point (BP) Method for Distillation Columns", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("Frequently, distillation involves species that cover a relatively narrow range of K-values. A particularly effective procedure for this case was suggested by Friday and Smith and developed in detail by Wang and Henke. It is referred to as the bubble-point (BP) method because a new set of stage temperatures is computed during each iteration from the bubble-point equations. All equations are partitioned and solved sequentially except for the M equations, which are solved separately for each component by the tridiagonal-matrix technique.")

            IObj?.Paragraphs.Add("Specifications are conditions and stage location of feeds, stage pressures, flow rates of sidestreams (note that liquid distillate flow rate, if any, is designated as U1), heat-transfer rates for all stages except stage 1 (condenser) and stage N (reboiler), total stages, bubble-point reflux flow rate, and vapor distillate flow rate.")

            IObj?.Paragraphs.Add("To initiate the calculations, values of tear variables, Vj and Tj, are assumed. Generally, it is sufficient to establish an initial set of Vj values based on constant-molar interstage flows using the specified reflux, distillate, feed, and sidestream flows. Initial Tj values can be provided by computing the bubble-point temperature of an estimated bottoms product and the dew-point temperature of an assumed distillate product (or computing a bubble-point temperature if distillate is liquid, or a temperature in between the dew point and bubble point if distillate is both vapor and liquid), and then using linear interpolation for the other stage temperatures.")

            IObj?.Paragraphs.Add("To solve the Tridiagonal Matrix for <mi>x_{i}</mi> by the Thomas method, <mi>K_{i,j}</mi> values are required. When they are composition-dependent, initial assumptions for all <mi>x_{i,j}</mi> and <mi>y_{i,j}</mi> values are also needed, unless ideal K-values are employed initially. For each iteration, the computed set of <mi>x_{i,j}</mi> values for each stage are not likely to satisfy the summation constraint. Although not mentioned by Wang and Henke, it is advisable to normalize the set of computed <mi>x_{i,j}</mi> values by the relation")

            IObj?.Paragraphs.Add("<m>(x_{i,j})_{normalized}=\frac{x_{i,j}}{\sum\limits_{i=1}^{C}{x_{i,j}} }</m>")

            IObj?.Paragraphs.Add("New temperatures for the stages are obtained by bubble point calculations using normalized <mi>x_{i,j}</mi> values.")

            IObj?.Paragraphs.Add("Values of <mi>y_{i,j}</mi> are determined along with the calculation of stage temperatures using the E equations. With a consistent set of values for <mi>x_{i,j}</mi>, Tj, and <mi>y_{i,j}</mi>, molar enthalpies are computed for each liquid and vapor stream leaving a stage. Since F1, V1, U1, W1, and L1 are specified, V2 and the condenser duty are readily obtained. Reboiler duty is determined by")

            IObj?.Paragraphs.Add("<m>Q_N=\sum\limits_{j=1}^{N}{(F_jh_{F_j}-U_jh_{L_j}-W_jh_{V_j})}-\sum\limits_{j=1}^{N-1}{(Q_j-V_1h_{V_1}-L_Nh_{L_N})}</m>")

            IObj?.Paragraphs.Add("A new set of Vj tear variables is computed by applying a modified energy balance obtained by")

            IObj?.Paragraphs.Add("<m>\alpha _jV_j+\beta _jV_{j+1}=\gamma _j</m>")

            IObj?.Paragraphs.Add("where")

            IObj?.Paragraphs.Add("<m>\alpha _j = h_{L_{j-1}}-h_{V_j}</m>")

            IObj?.Paragraphs.Add("<m>\beta _j=h_{V_{j+1}}-h_{L_j}</m>")

            IObj?.Paragraphs.Add("<m>\gamma _j =\left[\sum\limits_{m=1}^{j-1}{(F_m-W_m-U_m)-V_1}\right](h_{L_j}-h_{L_{j=1}})+F_j(h_{L_j}-h_{F_j})+W_j(h_{V_j}-h_{L_j})+Q_j</m>")

            IObj?.Paragraphs.Add("and enthalpies are evaluated at the stage temperatures last computed rather than at those used to initiate the iteration.")

            IObj?.Paragraphs.Add("<m>V_j=\frac{\gamma _{j-1}-\alpha _{j-1}V_{j-1}}{\beta _{j-1}} </m>")

            IObj?.Paragraphs.Add("Corresponding liquid flow rates are obtained from")

            IObj?.Paragraphs.Add("<m>L_j=V_{j+1}+\sum\limits_{m=1}^{j}{(F_m-U_m-W_m)-V_1} </m>")

            IObj?.Paragraphs.Add("One convergence criterion is")

            IObj?.Paragraphs.Add("<m>\sum\limits_{j=1}^{N}{\left[\frac{T_j^{(k)}-T_j^{(k-1)}}{T_j^{(k)}}\right]^2+\sum\limits_{j=1}^{N}{\left[\frac{V_j^{(k)}-V_j^{(k-1)}}{V_j^{(k)}} \right]^2 }}\leq \in</m>")

            IObj?.Paragraphs.Add("where T is an absolute temperature and <mi>\in</mi> is some prescribed tolerance. However, Wang and Henke suggest that the following simpler criterion, which is based on successive sets of Tj values only, is adequate.")

            IObj?.Paragraphs.Add("Successive substitution is often employed for iterating the tear variables; that is, values of Tj and Vj are used directly to initiate the next iteration. It is desirable to inspect, and, if necessary, adjust the generated tear variables prior to beginning the next iteration.")

            IObj?.Paragraphs.Add("The BP convergence rate is unpredictable, and can depend on the assumed initial set of Tj values. Cases with high reflux ratios can be more difficult to converge than those with low ratios. Orbach and Crowe describe an extrapolation method for accelerating convergence based on periodic adjustment of the tear variables when their values form geometric progressions during at least four successive iterations.")

            IObj?.Paragraphs.Add("<h2>Input Parameters / Initial Estimates</h2>")

            IObj?.Paragraphs.Add(String.Format("Stage Temperatures: {0}", T.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Stage Pressures: {0}", P.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("Feeds: {0}", F.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Vapor Flows: {0}", V.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Liquid Flows: {0}", L.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Vapor Side Draws: {0}", VSS.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Liquid Side Draws: {0}", LSS.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("Mixture Compositions: {0}", z.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Liquid Phase Compositions: {0}", x.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Vapor/Liquid2 Phase Compositions: {0}", y.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("K-values: {0}", Kval.ToMathArrayString))

            IObj?.Paragraphs.Add("<h2>Calculated Parameters</h2>")

            Dim doparallel As Boolean = Settings.EnableParallelProcessing

            Dim ic As Integer
            Dim t_error, t_error_ant, vf_error, xcerror(ns) As Double
            Dim Tj(ns), Tj_ant(ns), dTj(ns) As Double
            Dim Fj(ns), Lj(ns), Vj(ns), Vj_ant(ns), dVj(ns), xc(ns)(), xc0(ns)(), fcj(ns)(), yc(ns)(), lc(ns)(), vc(ns)(), zc(ns)(), K(ns)(), Kant(ns)() As Double
            Dim Hfj(ns), Hv(ns), Hl(ns) As Double
            Dim VSSj(ns), LSSj(ns) As Double

            Dim rebabs As Boolean = False, refabs As Boolean = False

            If DirectCast(rc, DistillationColumn).ReboiledAbsorber Then rebabs = True
            If DirectCast(rc, DistillationColumn).RefluxedAbsorber Then refabs = True

            'step0

            Dim cv As New SystemsOfUnits.Converter
            Dim spval1, spval2 As Double

            spval1 = specs("C").SpecValue
            spval2 = specs("R").SpecValue

            Select Case specs("C").SType
                Case ColumnSpec.SpecType.Temperature,
                      ColumnSpec.SpecType.Product_Molar_Flow_Rate,
                      ColumnSpec.SpecType.Product_Mass_Flow_Rate,
                      ColumnSpec.SpecType.Heat_Duty,
                      ColumnSpec.SpecType.Component_Molar_Flow_Rate,
                      ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    spval1 = spval1.ConvertToSI(specs("C").SpecUnit)
                Case ColumnSpec.SpecType.Feed_Recovery
                    spval1 /= 100.0
            End Select

            Select Case specs("R").SType
                Case ColumnSpec.SpecType.Temperature,
                      ColumnSpec.SpecType.Product_Molar_Flow_Rate,
                      ColumnSpec.SpecType.Product_Mass_Flow_Rate,
                      ColumnSpec.SpecType.Heat_Duty,
                      ColumnSpec.SpecType.Component_Molar_Flow_Rate,
                      ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    spval2 = spval2.ConvertToSI(specs("R").SpecUnit)
                Case ColumnSpec.SpecType.Feed_Recovery
                    spval2 /= 100.0
            End Select

            'step1

            Dim rr, B, D2 As Double

            'step2

            Dim i, j As Integer

            For i = 0 To ns
                Array.Resize(fcj(i), nc)
                Array.Resize(xc(i), nc)
                Array.Resize(xc0(i), nc)
                Array.Resize(yc(i), nc)
                Array.Resize(lc(i), nc)
                Array.Resize(vc(i), nc)
                Array.Resize(zc(i), nc)
                Array.Resize(zc(i), nc)
                Array.Resize(K(i), nc)
                Array.Resize(Kant(i), nc)
            Next

            For i = 0 To ns
                VSSj(i) = VSS(i)
                LSSj(i) = LSS(i)
                Lj(i) = L(i)
                Vj(i) = V(i)
                Tj(i) = T(i)
                K(i) = Kval(i)
                Fj(i) = F(i)
                Hfj(i) = HF(i) / 1000
                fcj(i) = fc(i)
            Next

            Dim sumFHF As Double = 0
            Dim sumF As Double = 0
            Dim sumLSSHl As Double = 0
            Dim sumLSS As Double = 0
            Dim sumVSSHv As Double = 0
            Dim sumVSS As Double = 0
            For i = 0 To ns
                sumF += F(i)
                If i > 0 Then sumLSS += LSS(i)
                sumVSS += VSS(i)
                sumFHF += Fj(i) * Hfj(i)
            Next

            If doparallel Then

                Dim task1 As Task = TaskHelper.Run(Sub() Parallel.For(0, ns + 1,
                                                         Sub(ipar)
                                                             Hl(ipar) = pp.DW_CalcEnthalpy(x(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(x(ipar)) / 1000
                                                             Hv(ipar) = pp.DW_CalcEnthalpy(y(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * pp.AUX_MMM(y(ipar)) / 1000
                                                         End Sub),
                                                      Settings.TaskCancellationTokenSource.Token)
                task1.Wait()

            Else

                For i = 0 To ns
                    IObj?.SetCurrent
                    pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                    Hl(i) = pp.DW_CalcEnthalpy(x(i), Tj(i), P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(x(i)) / 1000
                    IObj?.SetCurrent
                    Hv(i) = pp.DW_CalcEnthalpy(y(i), Tj(i), P(i), PropertyPackages.State.Vapor) * pp.AUX_MMM(y(i)) / 1000
                Next

            End If

            IObj?.Paragraphs.Add(String.Format("Vapor Enthalpies: {0}", Hv.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Liquid Enthalpies: {0}", Hl.ToMathArrayString))

            If Not rebabs Then
                Select Case specs("C").SType
                    Case ColumnSpec.SpecType.Feed_Recovery
                        LSSj(0) = spval1 * F.SumY
                        rr = Lj(0) / LSSj(0)
                    Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                        LSSj(0) = spval1 / pp.AUX_MMM(x(0)) * 1000
                        rr = Lj(0) / LSSj(0)
                    Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                        LSSj(0) = spval1
                        rr = Lj(0) / LSSj(0)
                    Case ColumnSpec.SpecType.Stream_Ratio
                        rr = spval1
                    Case ColumnSpec.SpecType.Heat_Duty
                        Q(0) = spval1
                        LSSj(0) = -Lj(0) - (Q(0) - Vj(1) * Hv(1) - F(0) * Hfj(0) + (Vj(0) + VSSj(0)) * Hv(0)) / Hl(0)
                        rr = Lj(0) / LSSj(0)
                End Select
            Else
                LSSj(0) = 0.0
                rr = (Vj(1) + Fj(0)) / Vj(0) - 1
            End If

            If Not refabs Then
                Select Case specs("R").SType
                    Case ColumnSpec.SpecType.Feed_Recovery
                        B = spval2 * F.SumY
                    Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                        B = spval2 / pp.AUX_MMM(x(ns)) * 1000
                    Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                        B = spval2
                    Case ColumnSpec.SpecType.Heat_Duty
                        Q(ns) = -spval2
                        Dim sum3, sum4 As Double
                        sum3 = 0
                        sum4 = 0
                        For i = 0 To ns
                            sum3 += F(i) * Hfj(i) - LSSj(i) * Hl(i) - VSSj(i) * Hv(i)
                        Next
                        sum4 = 0
                        For i = 0 To ns - 1
                            sum4 += Q(i)
                        Next
                        B = (sum3 - sum4 - Vj(0) * Hv(0) - Q(ns)) / Hl(ns)
                End Select
            Else
                B = Lj(ns)
            End If

            If Not rebabs Then
                If condt = Column.condtype.Full_Reflux Then
                    Vj(0) = sumF - B - sumLSS - sumVSS
                    LSSj(0) = 0.0#
                Else
                    D2 = sumF - B - sumLSS - sumVSS - Vj(0)
                    LSSj(0) = D2
                End If
            Else
                Vj(0) = sumF - B - sumLSS - sumVSS
                LSSj(0) = 0.0#
                Lj(ns) = B
            End If

            Dim names = pp.RET_VNAMES().ToList()

            reporter?.AppendLine("========================================================")
            reporter?.AppendLine(String.Format("Initial Estimates"))
            reporter?.AppendLine("========================================================")
            reporter?.AppendLine()

            reporter?.AppendLine("Stage Conditions & Flows")
            reporter?.AppendLine(String.Format("{0,-16}{1,16}{2,16}{3,16}{4,16}{5,16}",
                                                   "Stage", "P (Pa)", "T (K)", "V (mol/s)",
                                                   "L (mol/s)", "LSS (mol/s)"))
            For i = 0 To ns
                reporter?.AppendLine(String.Format("{0,-16}{1,16:G6}{2,16:G6}{3,16:G6}{4,16:G6}{5,16:G6}",
                                                   i + 1, P(i), T(i), V(i), L(i), LSS(i)))
            Next

            reporter?.AppendLine()
            reporter?.AppendLine("Stage Molar Fractions - Vapor")
            reporter?.Append("Stage".PadRight(20))
            For j = 0 To nc - 1
                reporter?.Append(names(j).PadLeft(20))
            Next
            reporter?.Append(vbCrLf)
            For i = 0 To ns
                reporter?.Append((i + 1).ToString().PadRight(20))
                For j = 0 To nc - 1
                    reporter?.Append(y(i)(j).ToString("G6").PadLeft(20))
                Next
                reporter?.Append(vbCrLf)
            Next

            reporter?.AppendLine()
            reporter?.AppendLine("Stage Molar Fractions - Liquid 1")
            reporter?.Append("Stage".PadRight(20))
            For j = 0 To nc - 1
                reporter?.Append(names(j).PadLeft(20))
            Next
            reporter?.Append(vbCrLf)
            For i = 0 To ns
                reporter?.Append((i + 1).ToString().PadRight(20))
                For j = 0 To nc - 1
                    reporter?.Append(x(i)(j).ToString("G6").PadLeft(20))
                Next
                reporter?.Append(vbCrLf)
            Next

            reporter?.AppendLine()
            reporter?.AppendLine()

            'step3

            Dim af As Double = 1.0
            Dim maxDT As Double = 50.0

            Dim Kfac(ns) As Double

            Dim fx(ns), xtj(ns), dfdx(ns, ns), fxb(ns), xtjb(ns), dxtj(ns) As Double

            Dim t_error_hist As New List(Of Double)
            Dim dt_error_hist As New List(Of Double)

            'internal loop

            ic = 0
            Do

                IObj?.SetCurrent()

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj2, "", "Solve", "Bubble-Point (BP) Internal Loop #" & ic, "Wang-Henke Bubble-Point (BP) Method for Distillation", True)

                Dim il_err As Double = 0.0#
                Dim il_err_ant As Double = 0.0#
                Dim num, denom, x0, fx0 As New ArrayList

                'step4

                'find component liquid flows by the tridiagonal matrix method

                IObj2?.Paragraphs.Add(String.Format("Find component liquid flows by the tridiagonal matrix method"))
                IObj2?.Paragraphs.Add(String.Format("Calculating TDM A, B, C, D"))

                Dim at(nc - 1)(), bt(nc - 1)(), ct(nc - 1)(), dt(nc - 1)(), xt(nc - 1)() As Double

                For i = 0 To nc - 1
                    Array.Resize(at(i), ns + 1)
                    Array.Resize(bt(i), ns + 1)
                    Array.Resize(ct(i), ns + 1)
                    Array.Resize(dt(i), ns + 1)
                    Array.Resize(xt(i), ns + 1)
                Next

                For i = 0 To ns
                    For j = 1 To nc
                        If Double.IsNaN(K(i)(j - 1)) Or Double.IsInfinity(K(i)(j - 1)) Then K(i)(j - 1) = pp.AUX_PVAPi(j - 1, Tj(i)) / P(i)
                    Next
                Next

                Dim sum1(ns), sum2(ns) As Double

                For i = 0 To ns
                    sum1(i) = 0
                    sum2(i) = 0
                    For j = 0 To i
                        sum1(i) += Fj(j) - LSSj(j) - VSSj(j)
                    Next
                    If i > 0 Then
                        For j = 0 To i - 1
                            sum2(i) += Fj(j) - LSSj(j) - VSSj(j)
                        Next
                    End If
                Next

                For i = 0 To nc - 1
                    For j = 0 To ns
                        dt(i)(j) = -Fj(j) * fcj(j)(i)
                        If j < ns Then
                            bt(i)(j) = -(Vj(j + 1) + sum1(j) - Vj(0) + LSSj(j) + (Vj(j) + VSSj(j)) * K(j)(i))
                        Else
                            bt(i)(j) = -(sum1(j) - Vj(0) + LSSj(j) + (Vj(j) + VSSj(j)) * K(j)(i))
                        End If
                        'tdma solve
                        If j < ns Then ct(i)(j) = Vj(j + 1) * K(j + 1)(i)
                        If j > 0 Then at(i)(j) = Vj(j) + sum2(j) - Vj(0)
                    Next
                Next

                'solve matrices

                IObj2?.Paragraphs.Add(String.Format("Calling TDM Solver to calculate liquid phase compositions"))

                IObj2?.SetCurrent()

                IObj2?.Paragraphs.Add(String.Format("A: {0}", at.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("B: {0}", bt.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("C: {0}", ct.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("D: {0}", dt.ToMathArrayString))

                'tomich

                If doparallel Then

                    Dim t1 As Task = TaskHelper.Run(Sub() Parallel.For(0, nc,
                                                                 Sub(ipar)
                                                                     xt(ipar) = Tomich.TDMASolve(at(ipar), bt(ipar), ct(ipar), dt(ipar))
                                                                 End Sub),
                                                      Settings.TaskCancellationTokenSource.Token)
                    t1.Wait()

                Else
                    For i = 0 To nc - 1
                        IObj2?.SetCurrent()
                        IObj2?.Paragraphs.Add(String.Format("Calling TDM Solver for Stage #{0}...", i + 1))
                        xt(i) = Tomich.TDMASolve(at(i), bt(i), ct(i), dt(i))
                    Next
                End If

                IObj2?.SetCurrent()

                IObj2?.Paragraphs.Add(String.Format("TDM solved successfully."))

                Dim sumx(ns), sumy(ns) As Double

                For i = 0 To ns
                    sumx(i) = 0
                    For j = 0 To nc - 1
                        lc(i)(j) = xt(j)(i)
                        If lc(i)(j) < 0.0# Then
                            lc(i)(j) = -lc(i)(j)
                        End If
                        sumx(i) += lc(i)(j)
                    Next
                Next

                If ic > 0 Then
                    For i = 0 To ns
                        xcerror(i) = 0.0
                        For j = 0 To nc - 1
                            xc0(i)(j) = xc(i)(j)
                            If sumx(i) > 0.0# Then
                                xc(i)(j) = lc(i)(j) / sumx(i)
                            Else
                                xc(i)(j) = yc(i)(j) / K(i)(j)
                            End If
                        Next
                        xcerror(i) = lc(i).Sum - 1.0
                    Next
                Else
                    For i = 0 To ns
                        xc(i) = x(i)
                    Next
                End If

                IObj2?.Paragraphs.Add(String.Format("l: {0}", lc.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("L: {0}", Lj.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("x: {0}", xc.ToMathArrayString))

                Dim tmp As Object

                'calculate new temperatures

                For i = 0 To ns
                    Tj_ant(i) = Tj(i)
                Next

                IObj2?.Paragraphs.Add("Calculating new temperatures...")


                If doparallel Then

                    Dim t1 As Task = TaskHelper.Run(Sub()
                                                        Parallel.For(0, ns + 1,
                                                                 Sub(ipar)
                                                                     Try
                                                                         Dim tmpvar As Object = flashalgs(ipar).Flash_PV(xc(ipar), P(ipar), 0.0, Tj(ipar), pp, True, K(ipar))
                                                                         Tj(ipar) = tmpvar(4)
                                                                         Kant(ipar) = K(ipar)
                                                                         K(ipar) = tmpvar(6)
                                                                         If Tj(ipar) < 0.0 Or Double.IsNaN(Tj(ipar)) Then
                                                                             Tj(ipar) = Tj_ant(ipar)
                                                                             K(ipar) = Kant(ipar)
                                                                         End If
                                                                     Catch ex As Exception
                                                                         Throw New Exception(String.Format(pp.Flowsheet.GetTranslatedString("Error calculating bubble point temperature for stage {0} with P = {1} Pa and molar composition {2}"), ipar, P(ipar), xc(ipar).ToArrayString()), ex)
                                                                     End Try
                                                                 End Sub)
                                                    End Sub,
                                                      Settings.TaskCancellationTokenSource.Token)
                    t1.Wait()

                Else

                    For i = 0 To ns
                        IObj2?.SetCurrent
                        pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                        Try
                            tmp = pp.FlashBase.Flash_PV(xc(i), P(i), 0.0, Tj(i), pp, True, K(i))
                        Catch ex As Exception
                            Throw New Exception(String.Format(pp.Flowsheet.GetTranslatedString("Error calculating bubble point temperature for stage {0} with P = {1} Pa and molar composition {2}"), i, P(i), xc(i).ToArrayString()), ex)
                        End Try
                        Tj(i) = tmp(4)
                        Kant(i) = K(i)
                        K(i) = tmp(6)
                        If Tj(i) < 0.0 Or Double.IsNaN(Tj(i)) Then
                            Tj(i) = Tj_ant(i)
                            K(i) = Kant(i)
                        End If
                    Next
                End If

                Tj(0) = Tj(0) - _subcoolingdeltat

                dTj = Tj.SubtractY(Tj_ant)

                'check Kvalues

                If maxDT < 10 Then
                    For i = 0 To ns
                        Tj(i) = Tj_ant(i) + dTj(i)
                        If Tj(i) < 0.0 Or Double.IsNaN(Tj(i)) Then
                            Tj(i) = Tj_ant(i)
                            K(i) = Kant(i)
                        End If
                    Next
                Else
                    For i = 0 To ns
                        Tj(i) = Tj(i) / 2 + Tj_ant(i) / 2
                    Next
                End If

                For i = 0 To ns
                    If Double.IsNaN(Tj(i)) Or Double.IsInfinity(Tj(i)) Then
                        Tj(i) = Tj_ant(i)
                    End If
                    For j = 0 To nc - 1
                        If Double.IsNaN(K(i)(j)) Then K(i)(j) = pp.AUX_PVAPi(j, Tj(i)) / P(i)
                    Next
                Next

                IObj2?.Paragraphs.Add(String.Format("Updated Temperatures: {0}", Tj.ToMathArrayString))

                t_error_ant = t_error
                t_error = Tj.SubtractY(Tj_ant).AbsSqrSumY

                t_error_hist.Add(t_error)
                dt_error_hist.Add(t_error - t_error_ant)

                IObj2?.Paragraphs.Add(String.Format("Temperature error: {0}", t_error))

                For i = ns To 0 Step -1
                    sumy(i) = 0
                    For j = 0 To nc - 1
                        If i = ns Then
                            yc(i)(j) = K(i)(j) * xc(i)(j)
                        Else
                            yc(i)(j) = eff(i) * K(i)(j) * xc(i)(j) + (1 - eff(i)) * yc(i + 1)(j)
                        End If
                        sumy(i) += yc(i)(j)
                    Next
                Next

                For i = 0 To ns
                    For j = 0 To nc - 1
                        yc(i)(j) = yc(i)(j) / sumy(i)
                    Next
                Next

                IObj2?.Paragraphs.Add(String.Format("y: {0}", yc.ToMathArrayString))

                If doparallel Then

                    Dim t1 As Task = TaskHelper.Run(Sub() Parallel.For(0, ns + 1,
                                                                                     Sub(ipar)
                                                                                         Hl(ipar) = pp.DW_CalcEnthalpy(xc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(ipar)) / 1000
                                                                                     End Sub),
                                                      Settings.TaskCancellationTokenSource.Token)

                    Dim t2 As Task = TaskHelper.Run(Sub() Parallel.For(0, ns + 1,
                                                                                     Sub(ipar)
                                                                                         Hv(ipar) = pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(ipar)) / 1000
                                                                                     End Sub),
                                                      Settings.TaskCancellationTokenSource.Token)
                    Task.WaitAll(t1, t2)

                Else
                    For i = 0 To ns
                        IObj2?.SetCurrent
                        pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                        Hl(i) = pp.DW_CalcEnthalpy(xc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(i)) / 1000
                        IObj2?.SetCurrent
                        Hv(i) = pp.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(i)) / 1000
                    Next
                End If

                'handle specs

                If Not rebabs Then
                    Select Case specs("C").SType
                        Case ColumnSpec.SpecType.Feed_Recovery
                            LSSj(0) = spval1 * F.SumY
                            rr = Lj(0) / LSSj(0)
                        Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                            LSSj(0) = spval1 / pp.AUX_MMM(xc(0)) * 1000
                            rr = Lj(0) / LSSj(0)
                        Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                            LSSj(0) = spval1
                            rr = Lj(0) / LSSj(0)
                        Case ColumnSpec.SpecType.Stream_Ratio
                            rr = spval1
                        Case ColumnSpec.SpecType.Heat_Duty
                            Q(0) = spval1
                            LSSj(0) = -Lj(0) - (Q(0) - Vj(1) * Hv(1) - F(0) * Hfj(0) + (Vj(0) + VSSj(0)) * Hv(0)) / Hl(0)
                            rr = Lj(0) / LSSj(0)
                    End Select
                Else
                    LSSj(0) = 0.0
                    rr = (Vj(1) + Fj(0)) / Vj(0) - 1
                End If

                If Not refabs Then
                    Select Case specs("R").SType
                        Case ColumnSpec.SpecType.Feed_Recovery
                            B = spval2 * F.SumY
                        Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                            B = spval2 / pp.AUX_MMM(xc(ns)) * 1000
                        Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                            B = spval2
                        'Case ColumnSpec.SpecType.Stream_Ratio
                        '    B = Vj(ns) / spval2
                        Case ColumnSpec.SpecType.Heat_Duty
                            Q(ns) = -spval2
                            Dim sum3, sum4 As Double
                            sum3 = 0
                            sum4 = 0
                            For i = 0 To ns
                                sum3 += F(i) * Hfj(i) - LSSj(i) * Hl(i) - VSSj(i) * Hv(i)
                            Next
                            sum4 = 0
                            For i = 0 To ns - 1
                                sum4 += Q(i)
                            Next
                            B = (sum3 - sum4 - Vj(0) * Hv(0) - Q(ns)) / Hl(ns)
                    End Select
                Else
                    B = Lj(ns)
                End If

                sumF = 0
                sumLSS = 0
                sumVSS = 0
                For i = 0 To ns
                    sumF += F(i)
                    If i > 0 Then sumLSS += LSS(i)
                    sumVSS += VSS(i)
                Next

                If condt = Column.condtype.Full_Reflux Or rebabs Then
                    Vj(0) = sumF - B - sumLSS - sumVSS
                    If Vj(0) < 0 Then Vj(0) = -Vj(0)
                    LSSj(0) = 0.0
                Else
                    LSSj(0) = sumF - B - sumLSS - sumVSS - Vj(0)
                End If

                'reboiler and condenser heat duties

                Dim alpha(ns), beta(ns), gamma(ns) As Double

                For i = 0 To ns
                    sum1(i) = 0
                    sum2(i) = 0
                    For j = 0 To i
                        sum1(i) += Fj(j) - LSSj(j) - VSSj(j)
                    Next
                    If i > 0 Then
                        For j = 0 To i - 1
                            sum2(i) += Fj(j) - LSSj(j) - VSSj(j)
                        Next
                    End If
                Next

                For j = 1 To ns
                    gamma(j) = (sum2(j) - Vj(0)) * (Hl(j) - Hl(j - 1)) + Fj(j) * (Hl(j) - Hfj(j)) + VSSj(j) * (Hv(j) - Hl(j)) + Q(j)
                    alpha(j) = Hl(j - 1) - Hv(j)
                    If j < ns Then beta(j) = Hv(j + 1) - Hl(j)
                Next

                'solve matrices

                For i = 0 To ns
                    Vj_ant(i) = Vj(i)
                Next

                If Not rebabs Then
                    If Not condt = Column.condtype.Full_Reflux Then
                        Vj(0) = V(0)
                        Vj(1) = (rr + 1) * LSSj(0) - Fj(0) + Vj(0)
                    Else
                        Vj(1) = (rr + 1) * Vj(0) - Fj(0)
                    End If
                Else
                    Vj(1) = Lj(0) - F(0) + LSSj(0) + VSSj(0) + Vj(0)
                End If

                For i = 2 To ns
                    Vj(i) = (gamma(i - 1) - alpha(i - 1) * Vj(i - 1)) / beta(i - 1)
                    If Vj(i) < 0 Then Vj(i) = 0.0000000001
                Next

                For i = 1 To ns - 1
                    Vj(i) = eff(i) * Vj(i) + (1 - eff(i)) * Vj(i + 1)
                Next

                vf_error = Vj.SubtractY(Vj_ant).AbsSqrSumY

                'Ljs
                For i = 0 To ns
                    If i < ns Then
                        If i = 0 Then
                            If rebabs Then
                                Lj(0) = (Vj(1) * Hv(1) + F(0) * Hfj(0) - (Vj(0) + VSSj(0)) * Hv(0) - LSSj(0) * Hl(0)) / Hl(0)
                            Else
                                If LSSj(0) > 0.0 Then
                                    Lj(0) = rr * LSSj(0)
                                Else
                                    Lj(i) = Vj(i + 1) + sum1(i) - Vj(0)
                                End If
                            End If
                        Else
                            Lj(i) = Vj(i + 1) + sum1(i) - Vj(0)
                        End If
                    Else
                        Lj(i) = sum1(i) - Vj(0)
                    End If
                    If Lj(i) < 0.0# Then Lj(i) = 0.0001 * Fj.Sum
                Next

                IObj2?.Paragraphs.Add(String.Format("alpha: {0}", alpha.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("beta: {0}", beta.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("gamma: {0}", gamma.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("Updated L: {0}", Lj.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("Updated V: {0}", Vj.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("Updated LSS: {0}", LSSj.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("Updated VSS: {0}", VSSj.ToMathArrayString))

                'reboiler and condenser heat duties
                Select Case coltype
                    Case Column.ColType.DistillationColumn
                        If Not specs("C").SType = ColumnSpec.SpecType.Heat_Duty Then
                            Q(0) = (Vj(1) * Hv(1) + F(0) * Hfj(0) - (Lj(0) + LSSj(0)) * Hl(0) - (Vj(0) + VSSj(0)) * Hv(0))
                        End If
                        If Not specs("R").SType = ColumnSpec.SpecType.Heat_Duty Then
                            Dim sum3, sum4 As Double
                            sum3 = 0
                            sum4 = 0
                            For i = 0 To ns
                                sum3 += F(i) * Hfj(i) - LSSj(i) * Hl(i) - VSSj(i) * Hv(i)
                            Next
                            For i = 0 To ns - 1
                                sum4 += Q(i)
                            Next
                            Q(ns) = sum3 - sum4 - Vj(0) * Hv(0) - Lj(ns) * Hl(ns)
                        End If
                        If rebabs Then Q(0) = 0.0
                    Case Column.ColType.AbsorptionColumn
                        'use provided values
                    Case Column.ColType.RefluxedAbsorber
                        If Not specs("C").SType = ColumnSpec.SpecType.Heat_Duty Then
                            Q(0) = (Vj(1) * Hv(1) + F(0) * Hfj(0) - (Lj(0) + LSSj(0)) * Hl(0) - (Vj(0) + VSSj(0)) * Hv(0))
                        End If
                    Case Column.ColType.ReboiledAbsorber
                        If Not specs("R").SType = ColumnSpec.SpecType.Heat_Duty Then
                            Q(ns) = (Lj(ns - 1) * Hl(ns - 1) + F(ns) * Hfj(ns) - (Lj(ns) + LSSj(ns)) * Hl(ns) - (Vj(ns) + VSSj(ns)) * Hv(ns))
                        End If
                End Select

                IObj2?.Paragraphs.Add(String.Format("Updated Q: {0}", Q.ToMathArrayString))

                ic = ic + 1

                If Not IdealH And Not IdealK Then
                    If ic >= maxits Then

                        reporter?.AppendLine("========================================================")
                        reporter?.AppendLine("Error Function Progression")
                        reporter?.AppendLine("========================================================")
                        reporter?.AppendLine()

                        reporter?.AppendLine(String.Format("{0,-16}{1,26}", "Iteration", "Temperature Error"))
                        For i = 0 To t_error_hist.Count - 1
                            reporter?.AppendLine(String.Format("{0,-16}{1,26:G6}", i + 1, t_error_hist(i)))
                        Next

                        reporter?.AppendLine("========================================================")
                        reporter?.AppendLine("Convergence Error!")
                        reporter?.AppendLine("========================================================")
                        reporter?.AppendLine()

                        reporter?.AppendLine(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCMaxIterationsReached"))

                        If rc.CreateSolverConvergengeReport Then rc.ColumnSolverConvergenceReport = reporter.ToString()

                        Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCMaxIterationsReached"))

                    End If
                End If
                If Not Tj.IsValid Or Not Vj.IsValid Or Not Lj.IsValid Then

                    reporter?.AppendLine("========================================================")
                    reporter?.AppendLine("Error Function Progression")
                    reporter?.AppendLine("========================================================")
                    reporter?.AppendLine()

                    reporter?.AppendLine(String.Format("{0,-16}{1,26}", "Iteration", "Temperature Error"))
                    For i = 0 To t_error_hist.Count - 1
                        reporter?.AppendLine(String.Format("{0,-16}{1,26:G6}", i + 1, t_error_hist(i)))
                    Next

                    reporter?.AppendLine("========================================================")
                    reporter?.AppendLine("Convergence Error!")
                    reporter?.AppendLine("========================================================")
                    reporter?.AppendLine()

                    reporter?.AppendLine(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))

                    If rc.CreateSolverConvergengeReport Then rc.ColumnSolverConvergenceReport = reporter.ToString()

                    Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))

                End If

                If ic = stopatitnumber - 1 Then Exit Do

                pp.CurrentMaterialStream.Flowsheet.CheckStatus()

                Calculator.WriteToConsole("Bubble Point solver T error = " & t_error, 1)

                IObj2?.Close()

                reporter?.AppendLine("========================================================")
                reporter?.AppendLine(String.Format("Updated variables after iteration {0}", ic))
                reporter?.AppendLine("========================================================")
                reporter?.AppendLine()

                reporter?.AppendLine("Stage Conditions & Flows")
                reporter?.AppendLine(String.Format("{0,-16}{1,16}{2,16}{3,16}{4,16}{5,16}",
                                                   "Stage", "P (Pa)", "T (K)", "V (mol/s)",
                                                   "L (mol/s)", "LSS (mol/s)"))
                For i = 0 To ns
                    reporter?.AppendLine(String.Format("{0,-16}{1,16:G6}{2,16:G6}{3,16:G6}{4,16:G6}{5,16:G6}",
                                                   i + 1, P(i), Tj(i), Vj(i), Lj(i), LSSj(i)))
                Next

                reporter?.AppendLine()
                reporter?.AppendLine("Stage Molar Fractions - Vapor")
                reporter?.Append("Stage".PadRight(20))
                For j = 0 To nc - 1
                    reporter?.Append(names(j).PadLeft(20))
                Next
                reporter?.Append(vbCrLf)
                For i = 0 To ns
                    reporter?.Append((i + 1).ToString().PadRight(20))
                    For j = 0 To nc - 1
                        reporter?.Append(yc(i)(j).ToString("G6").PadLeft(20))
                    Next
                    reporter?.Append(vbCrLf)
                Next

                reporter?.AppendLine()
                reporter?.AppendLine("Stage Molar Fractions - Liquid 1")
                reporter?.Append("Stage".PadRight(20))
                For j = 0 To nc - 1
                    reporter?.Append(names(j).PadLeft(20))
                Next
                reporter?.Append(vbCrLf)
                For i = 0 To ns
                    reporter?.Append((i + 1).ToString().PadRight(20))
                    For j = 0 To nc - 1
                        reporter?.Append(xc(i)(j).ToString("G6").PadLeft(20))
                    Next
                    reporter?.Append(vbCrLf)
                Next

                reporter?.AppendLine()
                reporter?.AppendLine()
                reporter?.AppendLine(String.Format("Temperature Error: {0} [tol: {1}]", t_error, tolerance * ns / 100))
                reporter?.AppendLine()
                reporter?.AppendLine()

            Loop Until (t_error + vf_error) < tolerance And ic > 1

            'check mass balance
            For i = 0 To ns
                If Math.Abs(yc(i).SumY - 1.0) > 0.001 Then
                    Throw New Exception("Could not converge to a valid solution")
                End If
                If Math.Abs(xc(i).SumY - 1.0) > 0.001 Then
                    Throw New Exception("Could not converge to a valid solution")
                End If
                If Lj(i) < 0.0 Or Vj(i) < 0.0 Or LSSj(i) < 0.0 Then
                    Throw New Exception("Could not converge to a valid solution. Please check the column specs")
                End If
            Next

            IObj?.Paragraphs.Add("The algorithm converged in " & ic & " iterations.")

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("Final converged values for T: {0}", Tj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for V: {0}", Vj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for L: {0}", Lj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for VSS: {0}", VSSj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for LSS: {0}", LSSj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for y: {0}", yc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for x: {0}", xc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for K: {0}", K.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for Q: {0}", Q.ToMathArrayString))

            IObj?.Close()

            reporter?.AppendLine("========================================================")
            reporter?.AppendLine("Error Function Progression")
            reporter?.AppendLine("========================================================")
            reporter?.AppendLine()

            reporter?.AppendLine(String.Format("{0,-16}{1,26}", "Iteration", "Temperature Error"))
            For i = 0 To t_error_hist.Count - 1
                reporter?.AppendLine(String.Format("{0,-16}{1,26:G6}", i + 1, t_error_hist(i)))
            Next

            If rc.CreateSolverConvergengeReport Then rc.ColumnSolverConvergenceReport = reporter.ToString()

            'finished, return arrays

            Return New Object() {Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ic, t_error}

        End Function

        Public Overrides Function SolveColumn(input As ColumnSolverInputData) As ColumnSolverOutputData

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Dim nc = input.NumberOfCompounds
            Dim ns = input.NumberOfStages
            Dim maxits = input.MaximumIterations
            Dim tol = input.Tolerances.ToArray()
            Dim F = input.FeedFlows.ToArray()
            Dim V = input.VaporFlows.ToArray()
            Dim L = input.LiquidFlows.ToArray()
            Dim VSS = input.VaporSideDraws.ToArray()
            Dim LSS = input.LiquidSideDraws.ToArray()
            Dim Kval = input.Kvalues.ToArray()
            Dim Q = input.StageHeats.ToArray()
            Dim x = input.LiquidCompositions.ToArray()
            Dim y = input.VaporCompositions.ToArray()
            Dim z = input.OverallCompositions.ToArray()
            Dim fc = input.FeedCompositions.ToArray()
            Dim HF = input.FeedEnthalpies.ToArray()
            Dim T = input.StageTemperatures.ToArray()
            Dim P = input.StagePressures.ToArray()
            Dim eff = input.StageEfficiencies.ToArray()

            Dim L1trials = input.L1trials
            Dim L2trials = input.L2trials
            Dim x1trials = input.x1trials
            Dim x2trials = input.x2trials

            Dim col = input.ColumnObject

            Dim llextractor As Boolean = False
            Dim myabs As AbsorptionColumn = TryCast(col, AbsorptionColumn)
            If myabs IsNot Nothing Then
                If CType(col, AbsorptionColumn).OperationMode = AbsorptionColumn.OpMode.Absorber Then
                    llextractor = False
                Else
                    llextractor = True
                End If
            End If

            _subcoolingdeltat = input.SubcoolingDeltaT

            Dim result As Object() = Solve(col, nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval, x, y, z, fc, HF, T, P,
                               input.CondenserType, input.EarlyStopIteration, eff, input.ColumnType, col.PropertyPackage, col.Specs, False, False)

            Dim output As New ColumnSolverOutputData

            'Return New Object() {Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ic, t_error}

            T = result(0)
            V = result(1)
            L = result(2)
            VSS = result(3)
            LSS = result(4)
            y = result(5)
            x = result(6)
            Kval = result(7)
            Q = result(8)

            Dim spci1 = col.Specs("C").ComponentIndex
            Dim spci2 = col.Specs("R").ComponentIndex

            Select Case col.Specs("C").SType
                Case ColumnSpec.SpecType.Component_Fraction
                    If col.CondenserType <> Column.condtype.Full_Reflux Then
                        If col.Specs("C").SpecUnit = "M" Or col.Specs("C").SpecUnit = "Molar" Then
                            col.Specs("C").CalculatedValue = x(0)(spci1)
                        Else 'W
                            col.Specs("C").CalculatedValue = col.PropertyPackage.AUX_CONVERT_MOL_TO_MASS(x(0))(spci1)
                        End If
                    Else
                        If col.Specs("C").SpecUnit = "M" Or col.Specs("C").SpecUnit = "Molar" Then
                            col.Specs("C").CalculatedValue = y(0)(spci1)
                        Else 'W
                            col.Specs("C").CalculatedValue = col.PropertyPackage.AUX_CONVERT_MOL_TO_MASS(y(0))(spci1)
                        End If
                    End If
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    If col.CondenserType <> Column.condtype.Full_Reflux Then
                        col.Specs("C").CalculatedValue = LSS(0) * x(0)(spci1) * col.PropertyPackage.RET_VMM()(spci1) / 1000
                    Else
                        col.Specs("C").CalculatedValue = V(0) * y(0)(spci1) * col.PropertyPackage.RET_VMM()(spci1) / 1000
                    End If
                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                    If col.CondenserType <> Column.condtype.Full_Reflux Then
                        col.Specs("C").CalculatedValue = LSS(0) * x(0)(spci1)
                    Else
                        col.Specs("C").CalculatedValue = V(0) * y(0)(spci1)
                    End If
                Case ColumnSpec.SpecType.Component_Recovery
                    Dim sumc As Double = 0
                    For j = 0 To ns
                        sumc += z(j)(spci1) * F(j)
                    Next
                    If col.CondenserType <> Column.condtype.Full_Reflux Then
                        col.Specs("C").CalculatedValue = LSS(0) * x(0)(spci1) / sumc * 100
                    Else
                        col.Specs("C").CalculatedValue = V(0) * y(0)(spci1) / sumc * 100
                    End If
                Case ColumnSpec.SpecType.Temperature
                    col.Specs("C").CalculatedValue = T(0)
                Case ColumnSpec.SpecType.Stream_Ratio
                    col.Specs("C").CalculatedValue = L(0) / LSS(0)
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    col.Specs("C").CalculatedValue = LSS(0) * DirectCast(col.PropertyPackage, PropertyPackages.PropertyPackage).AUX_MMM(x(0)) / 1000
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                    col.Specs("C").CalculatedValue = LSS(0)
                Case ColumnSpec.SpecType.Heat_Duty
                    col.Specs("C").CalculatedValue = Q(0)
                Case ColumnSpec.SpecType.Feed_Recovery
                    col.Specs("C").CalculatedValue = LSS(0) / F.SumY * 100.0
            End Select

            Select Case col.Specs("R").SType
                Case ColumnSpec.SpecType.Component_Fraction
                    If col.Specs("R").SpecUnit = "M" Or col.Specs("R").SpecUnit = "Molar" Then
                        col.Specs("R").CalculatedValue = x(ns)(spci1)
                    Else 'W
                        col.Specs("R").CalculatedValue = col.PropertyPackage.AUX_CONVERT_MOL_TO_MASS(x(ns))(spci2)
                    End If
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    col.Specs("R").CalculatedValue = L(ns) * x(ns)(spci2) * col.PropertyPackage.RET_VMM()(spci2) / 1000
                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                    col.Specs("R").CalculatedValue = L(ns) * x(ns)(spci2)
                Case ColumnSpec.SpecType.Component_Recovery
                    Dim sumc As Double = 0
                    For j = 0 To ns
                        sumc += z(j)(spci2) * F(j)
                    Next
                    col.Specs("R").CalculatedValue = L(ns) * x(ns)(spci2) / sumc * 100
                Case ColumnSpec.SpecType.Temperature
                    col.Specs("R").CalculatedValue = T(ns)
                Case ColumnSpec.SpecType.Stream_Ratio
                    col.Specs("R").CalculatedValue = V(ns) / L(ns)
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    col.Specs("R").CalculatedValue = L(ns) * DirectCast(col.PropertyPackage, PropertyPackages.PropertyPackage).AUX_MMM(x(ns)) / 1000
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                    col.Specs("R").CalculatedValue = L(ns)
                Case ColumnSpec.SpecType.Heat_Duty
                    col.Specs("R").CalculatedValue = Q(ns)
                Case ColumnSpec.SpecType.Feed_Recovery
                    col.Specs("R").CalculatedValue = L(ns) / F.SumY * 100.0
            End Select

            With output
                .FinalError = result(10)
                .IterationsTaken = result(9)
                .StageTemperatures = DirectCast(result(0), Double()).ToList()
                .VaporFlows = DirectCast(result(1), Double()).ToList()
                .LiquidFlows = DirectCast(result(2), Double()).ToList()
                .VaporSideDraws = DirectCast(result(3), Double()).ToList()
                .LiquidSideDraws = DirectCast(result(4), Double()).ToList()
                .VaporCompositions = DirectCast(result(5), Double()()).ToList()
                .LiquidCompositions = DirectCast(result(6), Double()()).ToList()
                .Kvalues = DirectCast(result(7), Double()()).ToList()
                .StageHeats = DirectCast(result(8), Double()).ToList()
            End With

            Return output

        End Function

    End Class

End Namespace
