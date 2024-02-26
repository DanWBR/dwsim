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
Imports DWSIM.UnitOperations.UnitOperations
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.SepOps
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.SepOps.SolvingMethods
Imports DWSIM.UnitOperations.UnitOperations.Column
Imports System.Math

Namespace UnitOperations.Auxiliary.SepOps.SolvingMethods

    <System.Serializable()> Public Class NaphtaliSandholmMethod

        Inherits ColumnSolver

        Dim _IObj As Inspector.InspectorItem

        Sub New()

        End Sub

        Dim _nc, _ns As Integer
        Dim _VSS, _LSS As Double()
        Dim _spval1, _spval2 As Double
        Dim _spci1, _spci2 As Integer
        Dim _eff, _F, _Q, _P, _HF As Double()
        Dim _fc()() As Double
        Public _pp As PropertyPackages.PropertyPackage
        Dim _coltype As Column.ColType
        Dim _specs As Dictionary(Of String, SepOps.ColumnSpec)
        Dim _bx, _dbx As Double()
        Dim _condtype As DistillationColumn.condtype
        Dim llextr As Boolean = False
        Dim _Kval()() As Double
        Dim _maxtchange, _Tj_ant(), _maxT, _maxvc, _maxlc As Double
        Dim _scalef As Double
        Dim _subcoolingDeltaT As Double

        Private grad As Boolean = False

        Private ik, ih As Boolean

        Private _reporter As Text.StringBuilder

        Private _counter As Integer

        Private _names As String()

        Public Overrides ReadOnly Property Name As String
            Get
                Return "Napthali-Sandholm"
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String
            Get
                Return "Napthali-Sandholm Simultaneous Correction (SC) Solver"
            End Get
        End Property

        Public Function FunctionValue(ByVal xl() As Double) As Double()

            _IObj?.SetCurrent

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "FunctionValue", "Simultaneous Correction (SC) Method MESH Equations Calculator", "Naphtali-Sandholm Simultaneous Correction (SC) Method for Distillation, Absorption and Stripping", True)

            IObj?.SetCurrent()

            Dim nc, ns As Integer
            Dim i, j As Integer

            Dim x As Double() = xl

            IObj?.Paragraphs.Add(String.Format("Input Variables: {0}", xl.ToMathArrayString))

            Dim doparallel As Boolean = Settings.EnableParallelProcessing

            Dim VSS, LSS, F, Q, P, HF, eff As Double()
            Dim spval1, spval2, spfval1, spfval2 As Double
            Dim spci1, spci2 As Integer
            Dim coltype As Column.ColType = _coltype

            F = _F
            Q = _Q
            P = _P
            HF = _HF
            eff = _eff

            spval1 = _spval1
            spval2 = _spval2
            spci1 = _spci1
            spci2 = _spci2

            VSS = _VSS
            LSS = _LSS

            nc = _nc
            ns = _ns

            Dim Sl(ns), Sv(ns) As Double
            Dim sumF As Double = 0
            Dim sumLSS As Double = 0
            Dim sumVSS As Double = 0
            Dim Tj(ns), Tj_ant(ns), vc(ns)(), lc(ns)(), zc(ns)(), Vj(ns), Lj(ns), xc(ns)(), yc(ns)(), Kval(ns)(), fc(ns)() As Double

            For i = 0 To ns
                Array.Resize(lc(i), nc)
                Array.Resize(vc(i), nc)
                Array.Resize(zc(i), nc)
                Array.Resize(xc(i), nc)
                Array.Resize(yc(i), nc)
                Array.Resize(fc(i), nc)
                Array.Resize(Kval(i), nc)
            Next


            For i = 0 To ns
                Tj(i) = x(i * (2 * nc + 1)) * _maxT
                If Double.IsNaN(Tj(i)) Or Double.IsInfinity(Tj(i)) Then
                    Throw New Exception(_pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))
                End If
                For j = 0 To nc - 1
                    vc(i)(j) = x(i * (2 * nc + 1) + j + 1) * _maxvc
                    lc(i)(j) = x(i * (2 * nc + 1) + j + 1 + nc) * _maxlc
                Next
            Next

            _Tj_ant = Tj.Clone

            Dim VSSj(ns), LSSj(ns), Hv(ns), Hl(ns), Hv0(ns), Hl0(ns) As Double
            Dim sumvkj(ns), sumlkj(ns) As Double

            Dim M(ns, nc - 1), E(ns, nc - 1), H(ns), Hr(ns) As Double
            Dim M_ant(ns, nc - 1), E_ant(ns, nc - 1), H_ant(ns) As Double

            For i = 0 To ns
                VSSj(i) = VSS(i)
                If i > 0 Then LSSj(i) = LSS(i)
            Next

            For i = 0 To ns
                sumvkj(i) = 0
                sumlkj(i) = 0
                For j = 0 To nc - 1
                    sumvkj(i) += vc(i)(j)
                    sumlkj(i) += lc(i)(j)
                Next
                Vj(i) = sumvkj(i)
                Lj(i) = sumlkj(i)
            Next

            If _coltype <> ColType.AbsorptionColumn And _condtype = condtype.Total_Condenser Then
                sumvkj(0) = 0.0
                Vj(0) = 0.0
            End If

            For i = 0 To ns
                For j = 0 To nc - 1
                    If sumvkj(i) > 0.0 Then
                        yc(i)(j) = vc(i)(j) / sumvkj(i)
                    Else
                        yc(i)(j) = 0.0
                    End If
                Next
                For j = 0 To nc - 1
                    If sumlkj(i) > 0.0# Then
                        xc(i)(j) = lc(i)(j) / sumlkj(i)
                    Else
                        xc(i)(j) = yc(i)(j) / (_pp.AUX_PVAPi(j, Tj(i)) / P(i))
                    End If
                Next
            Next

            For i = 0 To ns
                zc(i) = _fc(i).NormalizeY()
                fc(i) = _fc(i).MultiplyConstY(F(i))
            Next

            sumF = 0
            sumLSS = 0
            sumVSS = 0
            For i = 0 To ns
                sumVSS += VSSj(i)
            Next
            For i = 1 To ns
                sumLSS += LSSj(i)
            Next
            If _coltype <> ColType.AbsorptionColumn And _condtype = Column.condtype.Full_Reflux Then
                LSSj(0) = 0.0#
            ElseIf _coltype <> ColType.AbsorptionColumn And _condtype = condtype.Total_Condenser Then
                LSSj(0) = vc(0).Sum
                yc(0) = _pp.DW_CalcBubT(xc(0), P(0), Tj(0), Nothing, False)(3)
            Else
                If llextr Then
                    LSSj(0) = F.Sum - Lj(ns) - sumLSS - sumVSS
                Else
                    LSSj(0) = 0.0
                End If
            End If

            For i = 0 To ns
                If Vj(i) > 0.0 Then Sv(i) = VSSj(i) / Vj(i) Else Sv(i) = 0
                If Lj(i) > 0.0 Then Sl(i) = LSSj(i) / Lj(i) Else Sl(i) = 0
            Next

            'calculate K-values

            If doparallel Then

                Dim task1 As Task = TaskHelper.Run(Sub() Parallel.For(0, ns + 1,
                                                         Sub(ipar)
                                                             Dim tmp0 As Object
                                                             If llextr Then
                                                                 tmp0 = _pp.DW_CalcKvalue(xc(ipar), yc(ipar), Tj(ipar), P(ipar), "LL")
                                                             Else
                                                                 If _pp.ShouldUseKvalueMethod3 Then
                                                                     If ipar = 0 And Math.Abs(_subcoolingDeltaT) > 0 Then
                                                                         tmp0 = _pp.DW_CalcKvalue3(xc(ipar).MultiplyConstY(Lj(ipar)), yc(ipar).MultiplyConstY(Vj(ipar)), Tj(ipar) - _subcoolingDeltaT, P(ipar))
                                                                     Else
                                                                         tmp0 = _pp.DW_CalcKvalue3(xc(ipar).MultiplyConstY(Lj(ipar)), yc(ipar).MultiplyConstY(Vj(ipar)), Tj(ipar), P(ipar))
                                                                     End If
                                                                 ElseIf _pp.ShouldUseKvalueMethod2 Then
                                                                     Dim zk = xc(ipar).MultiplyConstY(Lj(ipar)).AddY(yc(ipar).MultiplyConstY(Vj(ipar))).MultiplyConstY(1 / (Lj(ipar) + Vj(ipar)))
                                                                     If ipar = 0 And Math.Abs(_subcoolingDeltaT) > 0 Then
                                                                         tmp0 = _pp.DW_CalcKvalue(zk, Tj(ipar) - _subcoolingDeltaT, P(ipar))
                                                                     Else
                                                                         tmp0 = _pp.DW_CalcKvalue(zk, Tj(ipar), P(ipar))
                                                                     End If
                                                                 Else
                                                                     If ipar = 0 And Math.Abs(_subcoolingDeltaT) > 0 Then
                                                                         tmp0 = _pp.DW_CalcKvalue(xc(ipar), yc(ipar), Tj(ipar) - _subcoolingDeltaT, P(ipar))
                                                                     Else
                                                                         tmp0 = _pp.DW_CalcKvalue(xc(ipar), yc(ipar), Tj(ipar), P(ipar))
                                                                     End If
                                                                 End If
                                                             End If
                                                             Dim jj As Integer
                                                             For jj = 0 To nc - 1
                                                                 Kval(ipar)(jj) = tmp0(jj)
                                                             Next
                                                         End Sub),
                                                      Settings.TaskCancellationTokenSource.Token)
                task1.Wait()

            Else

                Dim tmp0 As Object
                For i = 0 To ns
                    IObj?.SetCurrent
                    If llextr Then
                        tmp0 = _pp.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i), "LL")
                    Else
                        If _pp.ShouldUseKvalueMethod3 Then
                            If i = 0 And Math.Abs(_subcoolingDeltaT) > 0 Then
                                tmp0 = _pp.DW_CalcKvalue3(xc(i).MultiplyConstY(Lj(i)), yc(i).MultiplyConstY(Vj(i)), Tj(i) - _subcoolingDeltaT, P(i))
                            Else
                                tmp0 = _pp.DW_CalcKvalue3(xc(i).MultiplyConstY(Lj(i)), yc(i).MultiplyConstY(Vj(i)), Tj(i), P(i))
                            End If
                        ElseIf _pp.ShouldUseKvalueMethod2 Then
                            Dim zk = xc(i).MultiplyConstY(Lj(i)).AddY(yc(i).MultiplyConstY(Vj(i))).MultiplyConstY(1 / (Lj(i) + Vj(i)))
                            If i = 0 And Math.Abs(_subcoolingDeltaT) > 0 Then
                                tmp0 = _pp.DW_CalcKvalue(zk, Tj(i) - _subcoolingDeltaT, P(i))
                            Else
                                tmp0 = _pp.DW_CalcKvalue(zk, Tj(i), P(i))
                            End If
                        Else
                            If i = 0 And Math.Abs(_subcoolingDeltaT) > 0 Then
                                tmp0 = _pp.DW_CalcKvalue(xc(i), yc(i), Tj(i) - _subcoolingDeltaT, P(i))
                            Else
                                tmp0 = _pp.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i))
                            End If
                        End If

                    End If
                    For j = 0 To nc - 1
                        Kval(i)(j) = tmp0(j)
                    Next
                    _pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                Next

            End If

            _Kval = Kval

            'calculate enthalpies

            If doparallel Then

                Dim task1 As Task = TaskHelper.Run(Sub() Parallel.For(0, ns + 1,
                                                         Sub(ipar)
                                                             If Vj(ipar) <> 0.0# Then
                                                                 If llextr Then
                                                                     Hv(ipar) = _pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * _pp.AUX_MMM(yc(ipar)) / 1000
                                                                 Else
                                                                     Hv(ipar) = _pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * _pp.AUX_MMM(yc(ipar)) / 1000
                                                                 End If
                                                             Else
                                                                 Hv(ipar) = 0.0#
                                                             End If
                                                             If Lj(ipar) <> 0 Then
                                                                 Hl(ipar) = _pp.DW_CalcEnthalpy(xc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * _pp.AUX_MMM(xc(ipar)) / 1000
                                                             Else
                                                                 Hl(ipar) = 0.0#
                                                             End If
                                                             Hr(ipar) = _pp.DW_CalcEnthalpyOfReaction(xc(ipar).MultiplyConstY(Lj(ipar)), Tj(ipar), P(ipar))
                                                         End Sub),
                                                      Settings.TaskCancellationTokenSource.Token)
                task1.Wait()

            Else

                For i = 0 To ns
                    If Vj(i) <> 0 Then
                        IObj?.SetCurrent
                        If llextr Then
                            Hv(i) = _pp.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * _pp.AUX_MMM(yc(i)) / 1000
                        Else
                            Hv(i) = _pp.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Vapor) * _pp.AUX_MMM(yc(i)) / 1000
                        End If
                    Else
                        Hv(i) = 0
                    End If
                    If Lj(i) <> 0 Then
                        IObj?.SetCurrent
                        Hl(i) = _pp.DW_CalcEnthalpy(xc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * _pp.AUX_MMM(xc(i)) / 1000
                    Else
                        Hl(i) = 0
                    End If
                    Hr(i) = _pp.DW_CalcEnthalpyOfReaction(xc(i).MultiplyConstY(Lj(i)), Tj(i), P(i))
                    _pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                Next

            End If

            'reboiler and condenser heat duties

            Select Case coltype
                Case Column.ColType.DistillationColumn
                    If Not _specs("C").SType = ColumnSpec.SpecType.Heat_Duty Then
                        i = 0
                        Q(0) = (Hl(i) * (1 + Sl(i)) * sumlkj(i) + Hv(i) * (1 + Sv(i)) * sumvkj(i) - Hv(i + 1) * sumvkj(i + 1) - HF(i) * F(i))
                        Q(0) = -Q(0)
                    End If
                    If Not _specs("R").SType = ColumnSpec.SpecType.Heat_Duty Then
                        i = ns
                        Q(ns) = (Hl(i) * (1 + Sl(i)) * sumlkj(i) + Hv(i) * (1 + Sv(i)) * sumvkj(i) - Hl(i - 1) * sumlkj(i - 1) - HF(i) * F(i))
                        Q(ns) = -Q(ns)
                    End If
                Case Column.ColType.AbsorptionColumn
                    'use provided values
                Case Column.ColType.RefluxedAbsorber
                    If Not _specs("C").SType = ColumnSpec.SpecType.Heat_Duty Then
                        i = 0
                        Q(0) = (Hl(i) * (1 + Sl(i)) * sumlkj(i) + Hv(i) * (1 + Sv(i)) * sumvkj(i) - Hv(i + 1) * sumvkj(i + 1) - HF(i) * F(i))
                        Q(0) = -Q(0)
                    End If
                Case Column.ColType.ReboiledAbsorber
                    If Not _specs("R").SType = ColumnSpec.SpecType.Heat_Duty Then
                        i = ns
                        Q(ns) = (Hl(i) * (1 + Sl(i)) * sumlkj(i) + Hv(i) * (1 + Sv(i)) * sumvkj(i) - Hl(i - 1) * sumlkj(i - 1) - HF(i) * F(i))
                        Q(ns) = -Q(ns)
                    End If
            End Select

            'handle user specs

            Select Case _specs("C").SType
                Case ColumnSpec.SpecType.Component_Fraction
                    If _condtype <> Column.condtype.Full_Reflux Then
                        If _specs("C").SpecUnit = "M" Or _specs("C").SpecUnit = "Molar" Then
                            spfval1 = Log(xc(0)(spci1) / spval1)
                            _specs("C").CalculatedValue = xc(0)(spci1)
                        Else 'W
                            spfval1 = Log((_pp.AUX_CONVERT_MOL_TO_MASS(xc(0))(spci1)) / spval1)
                            _specs("C").CalculatedValue = _pp.AUX_CONVERT_MOL_TO_MASS(xc(0))(spci1)
                        End If
                    Else
                        If _specs("C").SpecUnit = "M" Or _specs("C").SpecUnit = "Molar" Then
                            spfval1 = Log((yc(0)(spci1)) / spval1)
                            _specs("C").CalculatedValue = yc(0)(spci1)
                        Else 'W
                            spfval1 = Log((_pp.AUX_CONVERT_MOL_TO_MASS(yc(0))(spci1)) / spval1)
                            _specs("C").CalculatedValue = (_pp.AUX_CONVERT_MOL_TO_MASS(yc(0))(spci1))
                        End If
                    End If
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    If _condtype <> Column.condtype.Full_Reflux Then
                        spfval1 = Log((LSSj(0) * xc(0)(spci1) * _pp.RET_VMM()(spci1) / 1000) / spval1)
                        _specs("C").CalculatedValue = (LSSj(0) * xc(0)(spci1) * _pp.RET_VMM()(spci1) / 1000)
                    Else
                        spfval1 = Log((Vj(0) * yc(0)(spci1) * _pp.RET_VMM()(spci1) / 1000) / spval1)
                        _specs("C").CalculatedValue = (Vj(0) * yc(0)(spci1) * _pp.RET_VMM()(spci1) / 1000)
                    End If
                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                    spfval1 = LSSj(0) * xc(0)(spci1) - spval1
                    If _condtype <> Column.condtype.Full_Reflux Then
                        spfval1 = Log((LSSj(0) * xc(0)(spci1)) / spval1)
                        _specs("C").CalculatedValue = LSSj(0) * xc(0)(spci1)
                    Else
                        spfval1 = Log((Vj(0) * yc(0)(spci1)) / spval1)
                        _specs("C").CalculatedValue = Vj(0) * yc(0)(spci1)
                    End If
                Case ColumnSpec.SpecType.Component_Recovery
                    Dim rec As Double = spval1 / 100
                    Dim sumc As Double = 0
                    For j = 0 To ns
                        sumc += zc(j)(spci1) * F(j)
                    Next
                    If _condtype <> Column.condtype.Full_Reflux Then
                        spfval1 = Log(LSSj(0) * xc(0)(spci1) / sumc / rec)
                        _specs("C").CalculatedValue = LSSj(0) * xc(0)(spci1) / sumc
                    Else
                        spfval1 = Log(Vj(0) * yc(0)(spci1) / sumc / rec)
                        _specs("C").CalculatedValue = Vj(0) * yc(0)(spci1) / sumc
                    End If
                Case ColumnSpec.SpecType.Heat_Duty
                    Q(0) = spval1
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    spfval1 = Log(LSSj(0) / (spval1 / _pp.AUX_MMM(xc(0)) * 1000))
                    _specs("C").CalculatedValue = LSSj(0) / _pp.AUX_MMM(xc(0)) / 1000
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                    spfval1 = Log(LSSj(0) / spval1)
                    _specs("C").CalculatedValue = LSSj(0)
                Case ColumnSpec.SpecType.Stream_Ratio
                    spfval1 = Log(Lj(0) / LSSj(0) / spval1)
                    _specs("C").CalculatedValue = Lj(0) / LSSj(0)
                Case ColumnSpec.SpecType.Temperature
                    spfval1 = Log((Tj(0)) / spval1)
                    _specs("C").CalculatedValue = Tj(0)
                Case ColumnSpec.SpecType.Feed_Recovery
                    spfval1 = Log(LSSj(0) / (spval1 / 100 * F.SumY))
                    _specs("C").CalculatedValue = LSSj(0) / F.SumY * 100.0
            End Select

            Select Case _specs("R").SType
                Case ColumnSpec.SpecType.Component_Fraction
                    If _specs("C").SpecUnit = "M" Or _specs("C").SpecUnit = "Molar" Then
                        spfval2 = Log(xc(ns)(spci2) / spval2)
                        _specs("R").CalculatedValue = xc(ns)(spci2)
                    Else 'W
                        spfval2 = Log((_pp.AUX_CONVERT_MOL_TO_MASS(xc(ns))(spci2)) / spval2)
                        _specs("R").CalculatedValue = _pp.AUX_CONVERT_MOL_TO_MASS(xc(ns))(spci2)
                    End If
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    spfval2 = Log((Lj(ns) * xc(ns)(spci2) * _pp.RET_VMM()(spci2) / 1000) / spval2)
                    _specs("R").CalculatedValue = (Lj(ns) * xc(ns)(spci2) * _pp.RET_VMM()(spci2) / 1000)
                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                    spfval2 = Log((Lj(ns) * xc(ns)(spci2)) / spval2)
                    _specs("R").CalculatedValue = (Lj(ns) * xc(ns)(spci2))
                Case ColumnSpec.SpecType.Component_Recovery
                    Dim rec As Double = spval2 / 100
                    Dim sumc As Double = 0
                    For j = 0 To ns
                        sumc += zc(j)(spci2) * F(j)
                    Next
                    spfval2 = Log(Lj(ns) * xc(ns)(spci2) / sumc / rec)
                    _specs("R").CalculatedValue = Lj(ns) * xc(ns)(spci2) / sumc
                Case ColumnSpec.SpecType.Heat_Duty
                    Q(ns) = spval2
                    _specs("R").CalculatedValue = spval2
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    spfval2 = Log(Lj(ns) / (spval2 / _pp.AUX_MMM(xc(ns)) * 1000))
                    _specs("R").CalculatedValue = Lj(ns) / (_pp.AUX_MMM(xc(ns)) * 1000)
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                    spfval2 = Log(Lj(ns) / spval2)
                    _specs("R").CalculatedValue = Lj(ns)
                Case ColumnSpec.SpecType.Stream_Ratio
                    spfval2 = Log(Vj(ns) / Lj(ns) / spval2)
                    _specs("R").CalculatedValue = Vj(ns) / Lj(ns)
                Case ColumnSpec.SpecType.Temperature
                    spfval2 = Log((Tj(ns)) / spval2)
                    _specs("R").CalculatedValue = Tj(ns)
                Case ColumnSpec.SpecType.Feed_Recovery
                    spfval2 = Log(Lj(ns) / (spval1 / 100 * F.SumY))
                    _specs("R").CalculatedValue = Lj(ns) / F.SumY * 100.0
            End Select

            For i = 0 To ns
                For j = 0 To nc - 1
                    M_ant(i, j) = M(i, j)
                    E_ant(i, j) = E(i, j)
                    H_ant(i) = H(i)
                    If i = 0 Then
                        If _coltype <> ColType.AbsorptionColumn Then
                            If _condtype = Column.condtype.Full_Reflux Then
                                M(i, j) = lc(i)(j) * (1 + Sl(i)) + vc(i)(j) * (1 + Sv(i)) - vc(i + 1)(j) - fc(i)(j)
                                E(i, j) = eff(i) * Kval(i)(j) * lc(i)(j) * sumvkj(i) / sumlkj(i) - vc(i)(j) + (1 - eff(i)) * vc(i + 1)(j) * sumvkj(i) / sumvkj(i + 1)
                            ElseIf _condtype = condtype.Partial_Condenser Then
                                M(i, j) = lc(i)(j) * (1 + Sl(i)) + vc(i)(j) * (1 + Sv(i)) - vc(i + 1)(j) - fc(i)(j)
                                E(i, j) = eff(i) * Kval(i)(j) * lc(i)(j) * sumvkj(i) / sumlkj(i) - vc(i)(j) + (1 - eff(i)) * vc(i + 1)(j) * sumvkj(i) / sumvkj(i + 1)
                            Else
                                'total condenser
                                Dim sum1 As Double = 0
                                For k = 0 To nc - 1
                                    sum1 += Kval(i)(k) * xc(i)(k)
                                Next
                                If j = 0 Then
                                    E(i, j) = 1 - sum1
                                Else
                                    E(i, j) = lc(i)(j) - Lj(0) / LSSj(0) * vc(i)(j)
                                End If
                                M(i, j) = lc(i)(j) * (1 + Sl(i)) - vc(i + 1)(j) - fc(i)(j)
                            End If
                        Else
                            M(i, j) = lc(i)(j) * (1 + Sl(i)) + vc(i)(j) * (1 + Sv(i)) - vc(i + 1)(j) - fc(i)(j)
                            E(i, j) = eff(i) * Kval(i)(j) * lc(i)(j) * sumvkj(i) / sumlkj(i) - vc(i)(j) + (1 - eff(i)) * vc(i + 1)(j) * sumvkj(i) / sumvkj(i + 1)
                        End If
                    ElseIf i = ns Then
                        M(i, j) = lc(i)(j) * (1 + Sl(i)) + vc(i)(j) * (1 + Sv(i)) - lc(i - 1)(j) - fc(i)(j)
                        E(i, j) = eff(i) * Kval(i)(j) * lc(i)(j) * sumvkj(i) / sumlkj(i) - vc(i)(j)
                    Else
                        M(i, j) = lc(i)(j) * (1 + Sl(i)) + vc(i)(j) * (1 + Sv(i)) - lc(i - 1)(j) - vc(i + 1)(j) - fc(i)(j)
                        E(i, j) = eff(i) * Kval(i)(j) * lc(i)(j) * sumvkj(i) / sumlkj(i) - vc(i)(j) + (1 - eff(i)) * vc(i + 1)(j) * sumvkj(i) / sumvkj(i + 1)
                    End If
                Next
                If i = 0 Then
                    H(i) = -Hr(i) + (Hl(i) * (1 + Sl(i)) * sumlkj(i) + Hv(i) * (1 + Sv(i)) * sumvkj(i) - Hv(i + 1) * sumvkj(i + 1) - HF(i) * F(i) - Q(i))
                ElseIf i = ns Then
                    H(i) = -Hr(i) + (Hl(i) * (1 + Sl(i)) * sumlkj(i) + Hv(i) * (1 + Sv(i)) * sumvkj(i) - Hl(i - 1) * sumlkj(i - 1) - HF(i) * F(i) - Q(i))
                Else
                    H(i) = -Hr(i) + (Hl(i) * (1 + Sl(i)) * sumlkj(i) + Hv(i) * (1 + Sv(i)) * sumvkj(i) - Hl(i - 1) * sumlkj(i - 1) - Hv(i + 1) * sumvkj(i + 1) - HF(i) * F(i) - Q(i))
                End If
                H(i) /= 1000.0
                Select Case coltype
                    Case Column.ColType.DistillationColumn
                        H(0) = spfval1 / spval1
                        H(ns) = spfval2 / spval2
                    Case Column.ColType.AbsorptionColumn
                        'do nothing
                    Case Column.ColType.ReboiledAbsorber
                        H(ns) = spfval2 / spval2
                    Case Column.ColType.RefluxedAbsorber
                        H(0) = spfval1 / spval1
                End Select
            Next

            Dim errors(x.Length - 1) As Double

            For i = 0 To ns
                errors(i * (2 * nc + 1)) = H(i)
                For j = 0 To nc - 1
                    errors(i * (2 * nc + 1) + j + 1) = M(i, j) * 1000000.0
                    errors(i * (2 * nc + 1) + j + 1 + nc) = E(i, j)
                Next
            Next

            IObj?.Paragraphs.Add(String.Format("M Equation Deviations: {0}", M.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("E Equation Deviations: {0}", E.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("H Equation Deviations: {0}", H.ToMathArrayString))

            Dim ersum = errors.AbsSqrSumY

            If Not grad Then


                _reporter?.AppendLine("==================================================================")
                _reporter?.AppendLine(String.Format("Internal Iteration {0} - Variables", _counter))
                _reporter?.AppendLine("==================================================================")
                _reporter?.AppendLine()

                _reporter?.AppendLine("Stage Conditions & Flows")
                _reporter?.AppendLine(String.Format("{0,-20}{1,20}{2,20}{3,20}{4,20}{5,20}",
                                                       "Stage", "P (Pa)", "T (K)", "V (mol/s)",
                                                       "L (mol/s)", "LSS (mol/s)"))
                For i = 0 To ns
                    _reporter?.AppendLine(String.Format("{0,-20}{1,20:G6}{2,20:G6}{3,20:G6}{4,20:G6}{5,20:G6}",
                                                       i + 1, P(i), Tj(i), Vj(i), Lj(i), LSSj(i)))
                Next

                _reporter?.AppendLine()
                _reporter?.AppendLine("Stage Molar Fractions - Vapor")
                _reporter?.Append("Stage".PadRight(20))
                For j = 0 To nc - 1
                    _reporter?.Append(_names(j).PadLeft(20))
                Next
                _reporter?.Append(vbCrLf)
                For i = 0 To ns
                    _reporter?.Append((i + 1).ToString().PadRight(20))
                    For j = 0 To nc - 1
                        _reporter?.Append(yc(i)(j).ToString("G6").PadLeft(20))
                    Next
                    _reporter?.Append(vbCrLf)
                Next

                _reporter?.AppendLine()
                _reporter?.AppendLine("Stage Molar Fractions - Liquid 1")
                _reporter?.Append("Stage".PadRight(20))
                For j = 0 To nc - 1
                    _reporter?.Append(_names(j).PadLeft(20))
                Next
                _reporter?.Append(vbCrLf)
                For i = 0 To ns
                    _reporter?.Append((i + 1).ToString().PadRight(20))
                    For j = 0 To nc - 1
                        _reporter?.Append(xc(i)(j).ToString("G6").PadLeft(20))
                    Next
                    _reporter?.Append(vbCrLf)
                Next

                _reporter?.AppendLine()
                _reporter?.AppendLine()

                _reporter?.AppendLine("==================================================================")
                _reporter?.AppendLine(String.Format("Internal Iteration {0} - MEH Equation Discrepancies", _counter))
                _reporter?.AppendLine("==================================================================")
                _reporter?.AppendLine()

                _reporter?.AppendLine("Stage Mass/Equilibrium/Enthalpy Balance Errors")
                _reporter?.AppendLine(String.Format("{0,-20}{1,20}{2,20}{3,20}",
                                                       "Stage", "M", "E", "H"))

                For i = 0 To ns
                    errors(i * (2 * nc + 1)) = H(i)
                    Dim e_error As Double = 0.0
                    Dim m_error As Double = 0.0
                    For j = 0 To nc - 1
                        m_error += M(i, j)
                        e_error += E(i, j)
                    Next
                    _reporter?.AppendLine(String.Format("{0,-20}{1,20:G6}{2,20:G6}{3,20:G6}", i + 1, m_error, e_error, H(i)))
                Next

                _reporter?.AppendLine()
                _reporter?.AppendLine()

                _reporter?.AppendLine(String.Format("Internal Iteration {0} - Absolute Sum of Square Errors: {1}", _counter, ersum))

                _reporter?.AppendLine()
                _reporter?.AppendLine()

                _pp.CurrentMaterialStream.Flowsheet.CheckStatus()

            End If

            IObj?.Paragraphs.Add(String.Format("Total Error: {0}", errors.AbsSqrSumY))

            IObj?.Close()

            If Double.IsNaN(ersum) Or Double.IsInfinity(ersum) Then
                Throw New Exception("Error evaluating error functions.")
            End If

            Return errors

        End Function

        Private Function FunctionGradient(epsilon As Double, ByVal x() As Double) As Double(,)

            grad = True

            Dim f1(), f2() As Double
            Dim g(x.Length - 1, x.Length - 1), x1(x.Length - 1), x2(x.Length - 1), dx(x.Length - 1), xbr(x.Length - 1), fbr(x.Length - 1) As Double
            Dim i, j, k, n As Integer

            n = x.Length - 1

            For i = 0 To x.Length - 1
                For j = 0 To x.Length - 1
                    If i <> j Then
                        x1(j) = x(j)
                        x2(j) = x(j)
                    Else
                        If x(j) = 0.0# Then
                            x1(j) = epsilon
                            x2(j) = 2 * epsilon
                        Else
                            x1(j) = x(j) * (1 - epsilon)
                            x2(j) = x(j) * (1 + epsilon)
                        End If
                    End If
                Next
                f1 = FunctionValue(x1)
                f2 = FunctionValue(x2)
                For k = 0 To x.Length - 1
                    g(k, i) = (f2(k) - f1(k)) / (x2(i) - x1(i))
                Next
            Next

            grad = False

            Return g

        End Function

        Public Function Solve(ByVal dc As Column, ByVal nc As Integer, ByVal ns As Integer, ByVal maxits As Integer,
                                ByVal tol As Double(), ByVal F As Double(), ByVal V As Double(),
                                ByVal Q As Double(), ByVal L As Double(),
                                ByVal VSS As Double(), ByVal LSS As Double(), ByVal Kval()() As Double,
                                ByVal x()() As Double, ByVal y()() As Double, ByVal z()() As Double,
                                ByVal fc()() As Double,
                                ByVal HF As Double(), ByVal T As Double(), ByVal P As Double(),
                                ByVal condt As DistillationColumn.condtype,
                                ByVal eff() As Double,
                                ByVal coltype As Column.ColType,
                                ByVal pp As PropertyPackages.PropertyPackage,
                                ByVal specs As Dictionary(Of String, SepOps.ColumnSpec),
                              ByVal CalcMode As Integer,
                              Optional ByVal LLEX As Boolean = False) As Object

            _names = pp.RET_VNAMES()

            Dim reporter As Text.StringBuilder = Nothing

            dc.ColumnSolverConvergenceReport = ""

            If dc.CreateSolverConvergengeReport Then reporter = New Text.StringBuilder()

            _reporter = reporter

            Dim esolv As IExternalNonLinearSystemSolver = Nothing
            If dc.FlowSheet.ExternalSolvers.ContainsKey(dc.ExternalSolverID) Then
                esolv = dc.FlowSheet.ExternalSolvers(dc.ExternalSolverID)
            End If

            'If CalcMode = 0 And esolv Is Nothing Then
            Try
                'run 4 iterations of the bubble point method to enhance the initial estimates.
                'if it doesn't suceeed, go on with the original estimates.

                Dim result = New WangHenkeMethod().Solve(dc, nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval,
                                           x, y, z, fc, HF, T, P, condt, 1, eff,
                                           coltype, pp, specs, False, False)
                T = result(0)
                V = result(1)
                L = result(2)
                VSS = result(3)
                LSS = result(4)
                y = result(5)
                x = result(6)
                Kval = result(7)
                Q = result(8)
            Catch ex As Exception
            End Try
            'End If

            _Tj_ant = T.Clone

            _maxtchange = 50

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Solve", "Simultaneous Correction (SC) Method", "Naphtali-Sandholm Simultaneous Correction (SC) Method for Distillation, Absorption and Stripping", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("BP and SR methods for vapor–liquid systems converge with difficulty or not at all for very nonideal liquid mixtures or for cases where the separator is like an absorber or stripper in one section and a fractionator in another section (e.g., reboiled absorber). Furthermore, BP and SR methods are generally restricted to limited specifications. Universal procedures for solving separation problems are based on the solution of the MESH equations, or combinations thereof, by simultaneous-correction (SC) techniques, which employ the Newton–Raphson (NR) method.")

            IObj?.Paragraphs.Add("To develop an SC procedure, it is necessary to select and order the unknown variables and corresponding functions (MESH equations). As discussed by Goldstein and Stanfield, grouping of functions by type is computationally most efficient for problems involving many components, but few stages. For problems involving many stages but relatively few components, it is most efficient to group the functions according to stage location. The latter grouping, presented here, is described by Naphtali and was implemented by Naphtali and Sandholm.")

            IObj?.Paragraphs.Add("The stage model is again employed. However, rather than solving the N(2C+3) MESH equations simultaneously, some equations are combined with the other MESH equations to eliminate 2N variables and thus reduce the problem to the simultaneous solution of N(2C+1) equations. This gives")

            IObj?.Paragraphs.Add("<m>V_j=\sum\limits_{i=1}^{C}{v_{i,j}}</m>")

            IObj?.Paragraphs.Add("<m>L_j=\sum\limits_{i=1}^{C}{l_{i,j}}</m>")

            IObj?.Paragraphs.Add("where mole-fraction definitions are used:")

            IObj?.Paragraphs.Add("<m>y_{i,j}=\frac{v_{i,j}}{V_j} </m>")

            IObj?.Paragraphs.Add("<m>x_{i,j}=\frac{l_{i,j}}{L_j} </m>")

            IObj?.Paragraphs.Add("As a result, the following N(2C+1) equations are obtained, where <mi>s_j=U_j/L_j</mi> and <mi>S_j=W_j/V_j</mi> are dimensionless sidestream flows.")

            IObj?.Paragraphs.Add("Material Balance")

            IObj?.Paragraphs.Add("<m>M_{i,j}=l_{i,j}(1+s_j)+v_{i,j}(1+S_j)-l_{i,j-1}-v_{i,j+1}-f_{i,j}=0</m>")

            IObj?.Paragraphs.Add("Phase Equilibria")

            IObj?.Paragraphs.Add("<m>E_{i,j}=K_{i,j}l_{i,j}\frac{\sum\limits_{k=1}^{C}{v_{k,j}} }{\sum\limits_{k=1}^{C}{l_{k,j}} }-v_{i,j}=0</m>")

            IObj?.Paragraphs.Add("Energy Balance")

            IObj?.Paragraphs.Add("<m>H_j=h_{L_j}(1+s_j)\sum\limits_{i=1}^{C}{l_{i,j}}+h_{V_j}(1+S_j)\sum\limits_{i=1}^{C}{v_{i,j}}-h_{L_{j-1}}\sum\limits_{i=1}^{C}{l_{i,j-1}}-h_{V_{j+1}}\sum\limits_{i=1}^{C}{v_{i,j+1}}-h_{F_j}\sum\limits_{i=1}^{C}{f_{i,j}}-Q_j=0</m>")

            IObj?.Paragraphs.Add("where <mi>f_{i,j}=F_jz_{i,j}</mi>.")

            IObj?.Paragraphs.Add("If N and all fi,j, TFj, PFj, Pj, sj, Sj and Qj are specified, the M, E, and H functions are nonlinear in the N(2C+1) unknown (output) variables yij, lij and Tj for i = 1 to C and j = 1 to N. Although other sets of specified and unknown variables are possible, this set is considered first.")

            IObj?.Paragraphs.Add("The above equations are solved simultaneously by the Newton–Raphson iterative method in which successive sets of the output variables are computed until the values of the M, E, and H functions are driven to within the convergence criteria or zero. During the iterations, nonzero values of the functions are called discrepancies or errors.")

            IObj?.Paragraphs.Add("Problem specifications are quite flexible. However, number of stages, and pressure, compositions, flow rates, and stage locations for all feeds are necessary specifications. The thermal condition of each feed can be given in terms of enthalpy, temperature, or fraction vaporized. A two-phase feed can be sent to the same stage or the vapor can be directed to the stage above. Stage pressures and stage efficiencies can be designated by specifying top- and bottom-stage values with remaining values obtained by linear interpolation. By default, intermediate stages are assumed adiabatic unless Qj or Tj values are specified. Vapor and/or liquid sidestreams can be designated in terms of total flow or flow rate of a specified component, or by the ratio of the sidestream flow to the flow rate passing to the next stage. The top- and bottom-stage specifications are selected from Q1 or QN, and/or from the other specifications.")

            IObj?.Paragraphs.Add("To achieve convergence, the Newton–Raphson procedure requires guesses for the values of all output variables. Rather than provide these a priori, they can be generated if T, V, and L are guessed for the bottom and top stages and, perhaps, for one or more intermediate stages. Remaining guessed Tj, Vj, and Lj values are obtained by linear interpolation of the Tj values and computed (Vj=Lj) values. Initial values for yij and lij are then obtained by either of two techniques. Based on initial guesses for all output variables, the sum of the squares of the discrepancy functions is compared to the convergence criterion")

            IObj?.Paragraphs.Add("<m>\tau _3=\sum\limits_{j=1}^{N}{\left\{(H_j)^2+\sum\limits_{i=1}^{C}{\left[(M_{i,j})^2+(E_{i,j})^2\right]} \right\} }\leq \epsilon _3</m>")

            IObj?.Paragraphs.Add("For all discrepancies to be of the same order of magnitude, it is necessary to divide energy-balance functions Hj by a scale factor approximating the latent heat of vaporization (e.g.,
1,000 Btu/lbmol). If the convergence criterion is")

            IObj?.Paragraphs.Add("<m>\epsilon _3 = N(2C+1)\left(\sum\limits_{j=1}^{N}{F_j^2} \right)10^{-10} </m>")

            IObj?.Paragraphs.Add("resulting converged values will generally be accurate, on the average, to four or more significant figures. When employing the above equation, most problems converge in 10 iterations or fewer. The convergence criterion is far from satisfied during the first iteration with guessed values for the output variables. For subsequent iterations, Newton–Raphson corrections can be added directly to the present values of the output variables to obtain a new set of output variables. Alternatively, a damping factor can be employed where t is a nonnegative, scalar step factor. At each iteration, a value of t is applied to all output variables. By permitting t to vary from slightly greater than 0 up to 2, it can dampen or accelerate convergence, as appropriate. For each iteration, a t that minimizes the sum of the squares is sought. Generally, optimal values of t proceed from an initial value for the second iteration at between 0 and 1 to a value nearly equal to or slightly greater than 1 when the criterion is almost satisfied. If there is no optimal value of t in the designated range, t can be set to 1, or some smaller value, and the sum of squares can be allowed to increase. Generally, after several iterations, the sum of squares decreases for every iteration.")

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

            llextr = LLEX 'liquid-liquid extractor

            Dim rebabs As Boolean = False, refabs As Boolean = False
            If TypeOf dc Is DistillationColumn Then
                If DirectCast(dc, DistillationColumn).ReboiledAbsorber Then coltype = Column.ColType.ReboiledAbsorber
                If DirectCast(dc, DistillationColumn).RefluxedAbsorber Then coltype = Column.ColType.RefluxedAbsorber
            End If

            Dim cv As New SystemsOfUnits.Converter
            Dim spval1, spval2 As Double
            Dim spci1, spci2 As Integer

            If TypeOf dc Is DistillationColumn Then
                Select Case specs("C").SType
                    Case ColumnSpec.SpecType.Component_Fraction,
                          ColumnSpec.SpecType.Stream_Ratio,
                          ColumnSpec.SpecType.Component_Recovery,
                          ColumnSpec.SpecType.Feed_Recovery
                        spval1 = specs("C").SpecValue
                    Case Else
                        spval1 = SystemsOfUnits.Converter.ConvertToSI(specs("C").SpecUnit, specs("C").SpecValue)
                End Select
                spci1 = specs("C").ComponentIndex
                Select Case specs("R").SType
                    Case ColumnSpec.SpecType.Component_Fraction,
                          ColumnSpec.SpecType.Stream_Ratio,
                          ColumnSpec.SpecType.Component_Recovery,
                          ColumnSpec.SpecType.Feed_Recovery
                        spval2 = specs("R").SpecValue
                    Case Else
                        spval2 = SystemsOfUnits.Converter.ConvertToSI(specs("R").SpecUnit, specs("R").SpecValue)
                End Select
                spci2 = specs("R").ComponentIndex
            End If

            Dim ic, ec As Integer
            Dim Tj(ns), Tj_ant(ns), T_(ns) As Double
            Dim Lj(ns), Vj(ns), xc(ns)(), yc(ns)(), lc(ns)(), vc(ns)(), zc(ns)() As Double
            Dim Tj0(ns), lc0(ns)(), vc0(ns)(), Kval0(ns)() As Double
            Dim i, j As Integer

            For i = 0 To ns
                Array.Resize(xc(i), nc)
                Array.Resize(yc(i), nc)
                Array.Resize(lc(i), nc)
                Array.Resize(vc(i), nc)
                Array.Resize(zc(i), nc)
                Array.Resize(lc0(i), nc)
                Array.Resize(vc0(i), nc)
            Next

            'step0

            'normalize initial estimates

            For i = 0 To ns
                HF(i) = HF(i) / 1000
            Next

            If V(0) = 0.0 Then V(0) = 0.0000000001

            Dim Sl(ns), Sv(ns), maxvc(ns), maxlc(ns) As Double

            For i = 0 To ns
                maxvc(i) = 0.0
                maxlc(i) = 0.0
                For j = 0 To nc - 1
                    vc(i)(j) = y(i)(j) * V(i)
                    lc(i)(j) = x(i)(j) * L(i)
                    xc(i)(j) = x(i)(j)
                    yc(i)(j) = y(i)(j)
                    Tj(i) = T(i)
                    If vc(i)(j) > maxvc(i) Then maxvc(i) = vc(i)(j)
                    If lc(i)(j) > maxlc(i) Then maxlc(i) = lc(i)(j)
                Next
                Sv(i) = VSS(i) / V(i)
                Sl(i) = LSS(i) / L(i)
            Next

            _maxT = Tj.Max
            _maxvc = maxvc.Max
            _maxlc = maxlc.Max

            If specs("C").SType = ColumnSpec.SpecType.Temperature Then Tj(0) = spval1
            If specs("R").SType = ColumnSpec.SpecType.Temperature Then Tj(ns) = spval2

            'step1

            Dim sumF As Double = 0
            Dim sumLSS As Double = 0
            Dim sumVSS As Double = 0
            For i = 0 To ns
                sumF += F(i)
                sumLSS += LSS(i)
                sumVSS += VSS(i)
            Next

            'step2

            Dim lsi, vsi As New ArrayList
            Dim el As Integer

            'size jacobian

            el = ns
            For i = 0 To ns
                If VSS(i) <> 0 Then
                    el += 1
                    vsi.Add(i)
                End If
                If LSS(i) <> 0 Then
                    el += 1
                    lsi.Add(i)
                End If
            Next

            Dim el_err As Double = 0.0#
            Dim el_err_ant As Double = 0.0#
            Dim il_err As Double()
            Dim il_err_ant As Double()

            'independent variables

            Dim VSSj(ns), LSSj(ns), Hv(ns), Hl(ns), Hv0(ns), Hl0(ns) As Double
            Dim sumvkj(ns), sumlkj(ns) As Double
            Dim fxvar((ns + 1) * (2 * nc + 1) - 1) As Double
            Dim xvar((ns + 1) * (2 * nc + 1) - 1), lb((ns + 1) * (2 * nc + 1) - 1), ub((ns + 1) * (2 * nc + 1) - 1) As Double

            For i = 0 To ns
                VSSj(i) = VSS(i)
                LSSj(i) = LSS(i)
            Next

            If coltype = ColType.DistillationColumn And condt = condtype.Total_Condenser Then
                vc(0) = lc(0).MultiplyConstY(LSS(0) / L(0))
            End If

            _nc = nc
            _ns = ns
            _VSS = VSS.Clone
            _LSS = LSS.Clone
            _spval1 = spval1
            _spval2 = spval2
            _spci1 = spci1
            _spci2 = spci2
            _eff = eff.Clone
            _F = F.Clone
            _Q = Q.Clone
            _P = P.Clone
            _HF = HF.Clone
            _fc = fc.Clone
            _pp = pp
            _coltype = coltype
            _specs = specs
            _condtype = condt

            For i = 0 To ns
                xvar(i * (2 * nc + 1)) = Tj(i) / _maxT
                For j = 0 To nc - 1
                    xvar(i * (2 * nc + 1) + j + 1) = vc(i)(j) / _maxvc
                    xvar(i * (2 * nc + 1) + j + 1 + nc) = lc(i)(j) / _maxlc
                Next
            Next

            For i = 0 To ub.Length - 1
                lb(i) = 1.0E-20
                ub(i) = 2.0
            Next

            IObj?.Paragraphs.Add("Creating variable vectors...")

            IObj?.Paragraphs.Add(String.Format("Initial Variable Values: {0}", xvar.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Lower Bounds: {0}", lb.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Upper Bounds: {0}", ub.ToMathArrayString))

            _IObj = IObj


            Dim names = pp.RET_VNAMES().ToList()

            reporter?.AppendLine("========================================================")
            reporter?.AppendLine(String.Format("Initial Estimates"))
            reporter?.AppendLine("========================================================")
            reporter?.AppendLine()

            reporter?.AppendLine("Stage Conditions & Flows")
            reporter?.AppendLine(String.Format("{0,-20}{1,20}{2,20}{3,20}{4,20}{5,20}",
                                                   "Stage", "P (Pa)", "T (K)", "V (mol/s)",
                                                   "L (mol/s)", "LSS (mol/s)"))
            For i = 0 To ns
                reporter?.AppendLine(String.Format("{0,-20}{1,20:G6}{2,20:G6}{3,20:G6}{4,20:G6}{5,20:G6}",
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

            grad = False

            _counter = 0

            Dim fx_error_hist As New List(Of Double)

            Dim obj As Double = 0.0#

            Dim n As Integer = xvar.Length - 1

            il_err_ant = FunctionValue(xvar)

            If Abs(il_err_ant.AbsSqrSumY) > tol.MinY_NonZero() Then
                If esolv IsNot Nothing Then
                    xvar = esolv.Solve(Function(xvars)
                                           _counter += 1
                                           Dim fval = FunctionValue(xvars)
                                           fx_error_hist.Add(fval.AbsSqrSumY())
                                           Return fval
                                       End Function,
                                       Function(xvars)
                                           Return FunctionGradient(0.001, xvars)
                                       End Function, Nothing, xvar, maxits, tol.MinY_NonZero())
                Else
                    Dim haderror As Boolean = True
                    Try
                        xvar = MathNet.Numerics.RootFinding.Broyden.FindRoot(Function(xvars)
                                                                                 _counter += 1
                                                                                 Dim fval = FunctionValue(xvars)
                                                                                 fx_error_hist.Add(fval.AbsSqrSumY())
                                                                                 Return fval
                                                                             End Function, xvar, tol.MinY_NonZero(), maxits)
                        haderror = False
                    Catch oex As OperationCanceledException
                        Throw oex
                    Catch ex As Exception
                    End Try
                    If haderror Then
                        Dim nsolv As New NewtonSolver()
                        nsolv.EnableDamping = True
                        nsolv.ExpandFactor = 1.6
                        nsolv.MaximumDelta = 0.2
                        nsolv.MaxIterations = maxits
                        nsolv.Tolerance = tol.MinY_NonZero()
                        nsolv.UseBroydenApproximation = True
                        Try
                            xvar = nsolv.Solve(Function(xvars)
                                                   Dim fval = FunctionValue(xvars)
                                                   fx_error_hist.Add(fval.AbsSqrSumY())
                                                   'pp.CurrentMaterialStream.Flowsheet.ShowMessage(dc.GraphicObject.Tag + ": [NR Solver] current objective function (error) value = " & fval.AbsSqrSumY, IFlowsheet.MessageType.Information)
                                                   _counter += 1
                                                   Return fval
                                               End Function,
                                               Function(xvars)
                                                   Return FunctionGradient(0.001, xvars)
                                               End Function, xvar)
                            haderror = False
                        Catch oex As OperationCanceledException
                            Throw oex
                        Catch ex As Exception
                        End Try
                        If haderror Then
                            nsolv.Reset()
                            nsolv.UseBroydenApproximation = False
                            Try
                                xvar = nsolv.Solve(Function(xvars)
                                                       Dim fval = FunctionValue(xvars)
                                                       fx_error_hist.Add(fval.AbsSqrSumY())
                                                       'pp.CurrentMaterialStream.Flowsheet.ShowMessage(dc.GraphicObject.Tag + ": [NR Solver] current objective function (error) value = " & fval.AbsSqrSumY, IFlowsheet.MessageType.Information)
                                                       _counter += 1
                                                       Return fval
                                                   End Function,
                                               Function(xvars)
                                                   Return FunctionGradient(0.001, xvars)
                                               End Function, xvar)
                            Catch ex As Exception
                                Throw ex
                            End Try
                        End If
                    End If
                End If
            End If

            IObj?.Paragraphs.Add(String.Format("Final Variable Values: {0}", xvar.ToMathArrayString))

            il_err = FunctionValue(xvar)

            pp.CurrentMaterialStream.Flowsheet.ShowMessage(dc.GraphicObject.Tag + ": [NR Solver] final objective function (error) value = " & il_err.AbsSqrSumY, IFlowsheet.MessageType.Information)

            Dim il_err_sum = il_err.AbsSqrSumY()

            If Abs(il_err_sum) > tol.MinY_NonZero() Then
                Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCErrorStillHigh"))
            End If

            For i = 0 To ns
                Tj(i) = xvar(i * (2 * nc + 1)) * _maxT
                For j = 0 To nc - 1
                    vc(i)(j) = xvar(i * (2 * nc + 1) + j + 1) * _maxvc
                    lc(i)(j) = xvar(i * (2 * nc + 1) + j + 1 + nc) * _maxlc
                Next
            Next

            For i = 0 To ns
                sumvkj(i) = 0
                sumlkj(i) = 0
                For j = 0 To nc - 1
                    sumvkj(i) += vc(i)(j)
                    sumlkj(i) += lc(i)(j)
                Next
                Vj(i) = sumvkj(i)
                Lj(i) = sumlkj(i)
            Next

            For i = 0 To ns
                For j = 0 To nc - 1
                    If sumlkj(i) > 0 Then
                        xc(i)(j) = lc(i)(j) / Lj(i)
                    End If
                Next
                For j = 0 To nc - 1
                    If Vj(i) > 0 Then
                        yc(i)(j) = vc(i)(j) / Vj(i)
                    Else
                        yc(i)(j) = xc(i)(j) * _Kval(i)(j)
                    End If
                Next
            Next

            ' finished, de-normalize and return arrays
            Dim K(ns)() As Double
            For i = 0 To ns
                For j = 0 To nc - 1
                    K(i) = _Kval(i)
                Next
            Next

            sumLSS = 0.0#
            sumVSS = 0.0#
            For i = 0 To ns
                sumVSS += VSSj(i)
            Next
            For i = 1 To ns
                sumLSS += LSSj(i)
            Next
            If coltype = ColType.DistillationColumn Then
                If condt = Column.condtype.Full_Reflux Then
                    Vj(0) = F.Sum - Lj(ns) - sumLSS - sumVSS
                    LSSj(0) = 0.0#
                Else
                    LSSj(0) = F.Sum - Lj(ns) - sumLSS - sumVSS - Vj(0)
                End If
            Else
                LSSj(0) = 0.0
            End If

            For i = 0 To ns
                If Vj(i) <> 0 Then Sv(i) = VSSj(i) / Vj(i) Else Sv(i) = 0
                If Lj(i) <> 0 Then Sl(i) = LSSj(i) / Lj(i) Else Sl(i) = 0
            Next

            Q = _Q.Clone

            For i = 0 To ns
                Lj(i) = sumlkj(i)
                If coltype = ColType.DistillationColumn And i = 0 Then
                    If condt = condtype.Total_Condenser Then
                        LSSj(0) = Vj(0)
                        Vj(0) = 0.0
                    Else
                        LSSj(0) = Sl(0) * Lj(0)
                        Vj(i) = sumvkj(0)
                    End If
                Else
                    LSSj(i) = Sl(i) * Lj(i)
                    Vj(i) = sumvkj(i)
                End If
                VSSj(i) = Sv(i) * Vj(i)
                F(i) = F(i)
                L(i) = L(i)
                V(i) = V(i)
                LSS(i) = LSS(i)
                VSS(i) = VSS(i)
                Q(i) = Q(i)
            Next

            IObj?.Paragraphs.Add("The algorithm converged in " & ec & " iterations.")

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

            For Each Ki In _Kval
                If pp.AUX_CheckTrivial(Ki) Then
                    IObj?.Paragraphs.Add("Invalid result - converged to the trivial solution.")
                    Throw New Exception("Invalid result - converged to the trivial solution.")
                End If
            Next

            IObj?.Close()

            reporter?.AppendLine("========================================================")
            reporter?.AppendLine("Error Function Progression")
            reporter?.AppendLine("========================================================")
            reporter?.AppendLine()

            reporter?.AppendLine(String.Format("{0,-16}{1,20}", "Iteration", "Error Funtion"))
            For i = 0 To fx_error_hist.Count - 1
                reporter?.AppendLine(String.Format("{0,-16}{1,20:G6}", i + 1, fx_error_hist(i)))
            Next

            reporter?.AppendLine()
            reporter?.AppendLine("Last Updated on " + Date.Now.ToString())

            Dim report As String = ""

            If dc.CreateSolverConvergengeReport Then dc.ColumnSolverConvergenceReport = reporter.ToString()

            Return New Object() {Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ec, il_err, ic, el_err}

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

            _subcoolingDeltaT = input.SubcoolingDeltaT

            Dim result As Object()

            result = Solve(col, nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval, x, y, z, fc, HF, T, P,
                               input.CondenserType, eff, input.ColumnType, col.PropertyPackage, col.Specs, input.CalculationMode,
                               llextractor)

            Dim output As New ColumnSolverOutputData

            'Return New Object() {Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ec, il_err, ic, el_err, dFdXvar}

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
            End Select

            With output
                .FinalError = DirectCast(result(10), Double()).AbsSqrSumY + result(12)
                .IterationsTaken = result(9) + result(11)
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
