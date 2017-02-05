'    Rigorous Column Solvers
'    Copyright 2008 Daniel Wagner O. de Medeiros
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
Imports DWSIM.MathOps.MathEx
Imports DWSIM.MathOps
Imports System.Math
Imports Mapack
Imports System.Threading.Tasks
Imports System.Linq
Imports DWSIM.Thermodynamics
Imports DWSIM.SharedClasses
Imports Cureos.Numerics
Imports DWSIM.Interfaces.Enums
Imports DotNumerics.Optimization

Namespace UnitOperations.Auxiliary.SepOps.SolvingMethods

    Public Enum ColSolvingMethod
        WangHenke_BubblePoint = 0
        NaphtaliSandholm_SimultaneousCorrection = 1
        Russell_InsideOut = 2
        Burningham_Otto_SumRates = 4
    End Enum

    <System.Serializable()> Public Class Tomich

        Public Shared Function TDMASolve(ByVal a As Double(), ByVal b As Double(), ByVal c As Double(), ByVal d As Double()) As Double()

            ' Warning: will modify c and d!

            Dim n As Integer = d.Length
            ' All arrays should be the same length
            Dim x As Double() = New Double(n - 1) {}
            Dim id As Double

            ' Modify the coefficients.

            c(0) /= b(0)
            ' Division by zero risk.
            d(0) /= b(0)
            ' Division by zero would imply a singular matrix.
            For i As Integer = 1 To n - 1
                id = b(i) - c(i - 1) * a(i)
                c(i) /= id
                ' This calculation during the last iteration is redundant.
                d(i) = (d(i) - d(i - 1) * a(i)) / id
            Next

            ' Now back substitute.

            x(n - 1) = d(n - 1)
            For i As Integer = n - 2 To 0 Step -1
                x(i) = d(i) - c(i) * x(i + 1)
            Next

            Return x

        End Function

    End Class

    <System.Serializable()> Public Class RussellMethod

        Sub New()

        End Sub

        Private Function CalcKbj1(ByVal ns As Integer, ByVal nc As Integer, ByVal K(,) As Object, _
                                        ByVal z()() As Double, ByVal y()() As Double, ByVal T() As Double, _
                                        ByVal P() As Double, ByRef pp As PropertyPackages.PropertyPackage) As Object

            Dim i, j As Integer

            Dim Kbj1(ns) As Object

            For i = 0 To ns
                Kbj1(i) = K(i, 0)
                For j = 1 To nc - 1
                    If Abs(K(i, j) - 1) < Abs(Kbj1(i) - 1) And z(i)(j) <> 0 Then Kbj1(i) = K(i, j)
                Next
            Next

            Return Kbj1

        End Function

        Private Function CalcKbj2(ByVal ns As Integer, ByVal nc As Integer, ByVal K(,) As Object, _
                                      ByVal z()() As Double, ByVal y()() As Double, ByVal T() As Double, _
                                      ByVal P() As Double, ByRef pp As PropertyPackages.PropertyPackage) As Object

            Dim i, j As Integer

            Dim Kbj1(ns) As Object
            Dim Kw11(ns)(), Kw21(ns)() As Double
            Dim wi(ns, nc - 1), ti(ns, nc - 1), sumwi(ns), sumti(ns) As Double
            For i = 0 To ns
                Array.Resize(Kw11(i), nc)
                Array.Resize(Kw21(i), nc)
            Next

            For i = 0 To ns
                Kw11(i) = pp.DW_CalcKvalue(z(i), T(i), P(i))
                Kw21(i) = pp.DW_CalcKvalue(z(i), T(i) + 0.1, P(i))
                pp.CurrentMaterialStream.Flowsheet.CheckStatus()
            Next

            For i = 0 To ns
                sumti(i) = 0
                For j = 0 To nc - 1
                    ti(i, j) = y(i)(j) * (Log(Kw21(i)(j)) - Log(Kw11(i)(j))) / (1 / (T(i) + 0.1) - 1 / T(i))
                    sumti(i) += Abs(ti(i, j))
                Next
            Next

            For i = 0 To ns
                If sumti(i) <> 0 Then
                    For j = 0 To nc - 1
                        wi(i, j) = Abs(ti(i, j)) / sumti(i)
                    Next
                Else
                    For j = 0 To nc - 1
                        wi(i, j) = z(i)(j)
                    Next
                End If
            Next

            For i = 0 To ns
                Kbj1(i) = 0
                For j = 0 To nc - 1
                    Kbj1(i) += wi(i, j) * Log(K(i, j))
                Next
                Kbj1(i) = Exp(Kbj1(i))
                If Kbj1(i) < 0 Then
                    Kbj1(i) = K(i, 0)
                    For j = 1 To nc - 1
                        If Abs(K(i, j) - 1) < Abs(Kbj1(i) - 1) Then Kbj1(i) = K(i, j)
                    Next
                End If
            Next

            Return Kbj1

        End Function

        Dim ndeps As Double = 0.01

        Dim _ns, _nc, _el As Integer
        Dim _Bj, _Aj, _Cj, _Dj, _Ej, _Fj, _eff, _T_, _Tj, _Lj, _Vj, _LSSj, _VSSj, _LSS, _VSS, _Rlj, _Rvj, _F, _P, _HF, _Q, Vjj As Double()
        Dim _S, _alpha As Double(,)
        Dim _fc, _xc, _yc, _lc, _vc, _zc As Double()()
        Dim _Kbj As Object()
        Dim _rr, _Sb, _maxF As Double
        Public _pp As PropertyPackages.PropertyPackage
        Public _ppr As PropertyPackages.RaoultPropertyPackage
        Dim _coltype As Column.ColType
        Dim _condtype As Column.condtype
        Dim _bx, _dbx As Double()
        Dim _vcnt, _lcnt As Integer
        Dim _specs As Dictionary(Of String, SepOps.ColumnSpec)
        Dim llextr As Boolean = False

        Private ik, ih As Boolean

        Private Function GetSolver(solver As OptimizationMethod) As SwarmOps.Optimizer

            Select Case solver
                Case OptimizationMethod.DifferentialEvolution
                    Return New SwarmOps.Optimizers.DE()
                Case OptimizationMethod.GradientDescent
                    Return New SwarmOps.Optimizers.GD()
                Case OptimizationMethod.LocalUnimodalSampling
                    Return New SwarmOps.Optimizers.LUS()
                Case OptimizationMethod.ManyOptimizingLiaisons
                    Return New SwarmOps.Optimizers.MOL()
                Case OptimizationMethod.Mesh
                    Return New SwarmOps.Optimizers.MESH()
                Case OptimizationMethod.ParticleSwarm
                    Return New SwarmOps.Optimizers.PS()
                Case OptimizationMethod.ParticleSwarmOptimization
                    Return New SwarmOps.Optimizers.PSO()
                Case Else
                    Return Nothing
            End Select

        End Function

        Public Function FunctionValue(ByVal x() As Double) As Double

            Dim errors(x.Length - 1) As Double

            Dim cv As New SystemsOfUnits.Converter
            Dim spval1, spval2, spfval1, spfval2 As Double
            Dim spci1, spci2 As Integer

            spval1 = SystemsOfUnits.Converter.ConvertToSI(_specs("C").SpecUnit, _specs("C").SpecValue)
            spci1 = _specs("C").ComponentIndex
            spval2 = SystemsOfUnits.Converter.ConvertToSI(_specs("R").SpecUnit, _specs("R").SpecValue)
            spci2 = _specs("R").ComponentIndex

            Dim sum1(_ns) As Double
            Dim i, j As Integer

            For i = 0 To _ns
                If i = 0 And _condtype <> Column.condtype.Full_Reflux Then
                    _Rlj(i) = Exp(x(i))
                Else
                    For j = 0 To _nc - 1
                        _S(i, j) = Exp(x(i)) * _alpha(i, j) * _Sb
                    Next
                End If
            Next

            Dim m1, m2 As Integer
            m1 = 0
            m2 = 0

            If _vcnt > 0 Then
                For i = _ns + 1 To _vcnt + _ns
                    For j = m1 To _ns
                        If _Rvj(j) <> 1 Then
                            m1 = j + 1
                            Exit For
                        End If
                    Next
                    _Rvj(m1 - 1) = Exp(x(i))
                Next
            End If

            If _lcnt > 0 Then
                For i = _vcnt + _ns + 1 To _vcnt + _lcnt + _ns
                    For j = m2 + 1 To _ns
                        If _Rlj(j) <> 1 Then
                            m2 = j + 1
                            Exit For
                        End If
                    Next
                    _Rlj(m2 - 1) = Exp(x(i))
                Next
            End If
            If _condtype = Column.condtype.Partial_Condenser Then
                For j = 0 To _nc - 1
                    _S(0, j) = Exp(x(_el)) * _alpha(0, j) * _Sb
                Next
            End If
            'step4

            'find component liquid flows by the tridiagonal matrix method

            Dim Bs(_ns, _nc - 1), Cs(_ns, _nc - 1) As Double
            Dim at(_nc - 1)(), bt(_nc - 1)(), ct(_nc - 1)(), dt(_nc - 1)(), xt(_nc - 1)() As Double

            For i = 0 To _nc - 1
                Array.Resize(at(i), _ns + 1)
                Array.Resize(bt(i), _ns + 1)
                Array.Resize(ct(i), _ns + 1)
                Array.Resize(dt(i), _ns + 1)
                Array.Resize(xt(i), _ns + 1)
            Next

            Dim ic0 As Integer = 0

            For i = 0 To _ns
                For j = 0 To _nc - 1
                    If i = 0 And _condtype = Column.condtype.Total_Condenser Then
                        Bs(i, j) = -(_Rlj(i))
                    Else
                        Bs(i, j) = -(_Rlj(i) + _S(i, j) * _Rvj(i))
                    End If
                    If i < _ns Then Cs(i, j) = _S(i + 1, j)
                Next
            Next

            For i = 0 To _nc - 1
                For j = 0 To _ns
                    dt(i)(j) = -_fc(j)(i) * _F(j)
                    bt(i)(j) = Bs(j, i)
                    If j < _ns Then ct(i)(j) = Cs(j, i)
                    If j > 0 Then at(i)(j) = 1
                Next
            Next

            'solve matrices

            'tomich
            For i = 0 To _nc - 1
                xt(i) = Tomich.TDMASolve(at(i), bt(i), ct(i), dt(i))
            Next

            For i = 0 To _ns
                Array.Resize(_xc(i), _nc)
                Array.Resize(_yc(i), _nc)
                Array.Resize(_lc(i), _nc)
                Array.Resize(_vc(i), _nc)
                Array.Resize(_zc(i), _nc)
            Next

            'step5

            For i = 0 To _ns
                _Lj(i) = 0
                For j = 0 To _nc - 1
                    _lc(i)(j) = xt(j)(i)
                    _Lj(i) += _lc(i)(j)
                Next
                'If _Lj(i) < 0 Then
                '    _Lj(i) = 1.0E-20
                'End If
            Next

            For i = 0 To _ns
                For j = 0 To _nc - 1
                    _xc(i)(j) = _lc(i)(j) / _Lj(i)
                Next
            Next

            For i = _ns To 0 Step -1
                _Vj(i) = 0
                For j = 0 To _nc - 1
                    If i < _ns Then
                        If _eff(i) <> 1.0 Then
                            _vc(i)(j) = _eff(i) * (_S(i, j) * _lc(i)(j) - _vc(i + 1)(j) * _Vj(i) / _Vj(i + 1)) + _vc(i + 1)(j) * _Vj(i) / _Vj(i + 1)
                        Else
                            _vc(i)(j) = _S(i, j) * _lc(i)(j)
                        End If
                    Else
                        _vc(i)(j) = _S(i, j) * _lc(i)(j)
                    End If
                    _Vj(i) += _vc(i)(j)
                Next
                'If _Vj(i) < 0 Then
                '    _Vj(i) = 1.0E-20
                'End If
            Next

            'departures from product flows

            Dim sumLSS As Double = 0
            Dim sumVSS As Double = 0
            Dim sumF As Double = 0
            For i = 0 To _ns
                If i > 0 Then sumLSS += _LSSj(i)
                sumVSS += _VSSj(i)
                sumF += _F(i)
            Next
            If _condtype = Column.condtype.Total_Condenser Then
                _LSSj(0) = sumF - sumLSS - sumVSS - _Lj(_ns)
                _Rlj(0) = 1 + _LSSj(0) / _Lj(0)
            ElseIf _condtype = Column.condtype.Partial_Condenser Then
                _LSSj(0) = sumF - sumLSS - sumVSS - _Vj(0) - _Lj(_ns)
                _Rlj(0) = 1 + _LSSj(0) / _Lj(0)
            Else
                _LSSj(0) = 0.0
                _Rlj(0) = 1
            End If
            'If _Lj(0) <> 0 Or Not Double.IsNaN(_Lj(0)) Or Not Double.IsInfinity(_Lj(0)) Then
            '    _Rlj(0) = 1 + _LSSj(0) / _Lj(0)
            'Else
            '    _Rlj(0) = 1
            'End If

            For i = 0 To _ns
                _VSSj(i) = (_Rvj(i) - 1) * _Vj(i)
                If i > 0 Then _LSSj(i) = (_Rlj(i) - 1) * _Lj(i)
            Next

            'For i = 0 To _ns
            '    sum1(i) = 0
            '    For j = 0 To i
            '        sum1(i) += _F(j) - _LSSj(j) - _VSSj(j)
            '    Next
            'Next

            ''Ljs
            'For i = 0 To _ns
            '    If i < _ns Then _Lj(i) = _Vj(i + 1) + sum1(i) - _Vj(0) Else _Lj(i) = sum1(i) - _Vj(0)
            '    If _Lj(i) < 0 Then
            '        _Lj(i) = 0.0000000001
            '    End If
            'Next

            For i = 0 To _ns
                For j = 0 To _nc - 1
                    If _Vj(i) <> 0 Then
                        _yc(i)(j) = _vc(i)(j) / _Vj(i)
                    Else
                        _yc(i)(j) = _vc(i)(j)
                    End If
                    _zc(i)(j) = (_lc(i)(j) + _vc(i)(j)) / (_Lj(i) + _Vj(i))
                Next
            Next

            Dim sum_axi(_ns) As Double

            For i = 0 To _ns
                sum_axi(i) = 0
                For j = 0 To _nc - 1
                    sum_axi(i) += _alpha(i, j) * _xc(i)(j)
                Next
            Next

            'step6

            'calculate new temperatures

            Dim _Tj_ant(_ns) As Double

            For i = 0 To _ns
                _Kbj(i) = 1 / sum_axi(i)
                _Tj_ant(i) = _Tj(i)
                _Tj(i) = _Bj(i) / (_Aj(i) - Log(_Kbj(i)))
                If Abs(_Tj(i) - _Tj_ant(i)) > 100 Or Double.IsNaN(_Tj(i)) Or Double.IsInfinity(_Tj(i)) Then
                    'switch to a bubble point temperature calculation...
                    If ik Then
                        Dim tmp = _ppr.DW_CalcBubT(_xc(i), _P(i), _Tj(i), Nothing, False)
                        _Tj(i) = tmp(4)
                    Else
                        Dim tmp = _pp.DW_CalcBubT(_xc(i), _P(i), _Tj(i), Nothing, False)
                        _Tj(i) = tmp(4)
                    End If
                    If Double.IsNaN(_Tj(i)) Or Double.IsInfinity(_Tj(i)) Then
                        If i > 0 Then _Tj(i) = _Tj_ant(i - 1)
                    End If
                    If Double.IsNaN(_Tj(i)) Or Double.IsInfinity(_Tj(i)) Then Throw New Exception(_pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))
                    _pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                End If
            Next

            'step7

            'calculate enthalpies

            Dim Hv(_ns), Hl(_ns), Hidv(_ns), Hidl(_ns), DHv(_ns), DHl(_ns) As Double

            For i = 0 To _ns
                Hidv(i) = _pp.RET_Hid(298.15, _Tj(i), _yc(i))
                Hidl(i) = _pp.RET_Hid(298.15, _Tj(i), _xc(i))
                DHv(i) = _Cj(i) + _Dj(i) * (_Tj(i) - _T_(i))
                DHl(i) = _Ej(i) + _Fj(i) * (_Tj(i) - _T_(i))
                Hv(i) = (Hidv(i) + DHv(i)) * _pp.AUX_MMM(_yc(i)) / 1000
                Hl(i) = (Hidl(i) + DHl(i)) * _pp.AUX_MMM(_xc(i)) / 1000
                'If llextr Then
                '    Hv(i) = _pp.DW_CalcEnthalpy(_yc(i), _Tj(i), _P(i), PropertyPackages.State.Liquid) * _pp.AUX_MMM(_yc(i)) / 1000
                'Else
                '    Hv(i) = _pp.DW_CalcEnthalpy(_yc(i), _Tj(i), _P(i), PropertyPackages.State.Vapor) * _pp.AUX_MMM(_yc(i)) / 1000
                'End If
                'Hl(i) = _pp.DW_CalcEnthalpy(_xc(i), _Tj(i), _P(i), PropertyPackages.State.Liquid) * _pp.AUX_MMM(_xc(i)) / 1000
                _pp.CurrentMaterialStream.Flowsheet.CheckStatus()
            Next

            'reboiler and condenser heat duties
            Select Case _coltype
                Case Column.ColType.DistillationColumn
                    If Not _specs("C").SType = ColumnSpec.SpecType.Heat_Duty Then
                        _Q(0) = Hl(0) * _Rlj(0) * _Lj(0) + Hv(0) * _Rvj(0) * _Vj(0) - Hv(1) * _Vj(1) - _HF(0) * _F(0)
                        _Q(0) = -_Q(0)
                    End If
                    If Not _specs("R").SType = ColumnSpec.SpecType.Heat_Duty Then
                        _Q(_ns) = Hl(_ns) * _Rlj(_ns) * _Lj(_ns) + Hv(_ns) * _Rvj(_ns) * _Vj(_ns) - Hl(_ns - 1) * _Lj(_ns - 1) - _HF(_ns) * _F(_ns)
                        _Q(_ns) = -_Q(_ns)
                    End If
                Case Column.ColType.AbsorptionColumn
                    'use provided values
                Case Column.ColType.RefluxedAbsorber
                    If Not _specs("C").SType = ColumnSpec.SpecType.Heat_Duty Then
                        _Q(0) = Hl(0) * _Rlj(0) * _Lj(0) + Hv(0) * _Rvj(0) * _Vj(0) - Hv(1) * _Vj(1) - _HF(0) * _F(0)
                        _Q(0) = -_Q(0)
                    End If
                Case Column.ColType.ReboiledAbsorber
                    If Not _specs("R").SType = ColumnSpec.SpecType.Heat_Duty Then
                        _Q(_ns) = Hl(_ns) * _Rlj(_ns) * _Lj(_ns) + Hv(_ns) * _Rvj(_ns) * _Vj(_ns) - Hl(_ns - 1) * _Lj(_ns - 1) - _HF(_ns) * _F(_ns)
                        _Q(_ns) = -_Q(_ns)
                    End If
            End Select

            'handle user specs

            'Condenser Specs
            Select Case _specs("C").SType
                Case ColumnSpec.SpecType.Component_Fraction
                    If _condtype <> Column.condtype.Full_Reflux Then
                        If _specs("C").SpecUnit = "M" Then
                            spfval1 = _xc(0)(spci1) - spval1
                        Else 'W
                            spfval1 = _pp.AUX_CONVERT_MOL_TO_MASS(_xc(0))(spci1) - spval1
                        End If
                    Else
                        If _specs("C").SpecUnit = "M" Then
                            spfval1 = _yc(0)(spci1) - spval1
                        Else 'W
                            spfval1 = _pp.AUX_CONVERT_MOL_TO_MASS(_yc(0))(spci1) - spval1
                        End If
                    End If
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    If _condtype <> Column.condtype.Full_Reflux Then
                        spfval1 = _LSSj(0) * _xc(0)(spci1) - spval1 / _pp.RET_VMM()(spci1) * 1000 / _maxF
                    Else
                        spfval1 = _Vj(0) * _yc(0)(spci1) - spval1 / _pp.RET_VMM()(spci1) * 1000 / _maxF
                    End If
                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                    If _condtype <> Column.condtype.Full_Reflux Then
                        spfval1 = _LSSj(0) * _xc(0)(spci1) - spval1 / _maxF
                    Else
                        spfval1 = _Vj(0) * _yc(0)(spci1) - spval1 / _maxF
                    End If
                Case ColumnSpec.SpecType.Component_Recovery
                    Dim rec As Double = spval1 / 100
                    Dim sumc As Double = 0
                    For j = 0 To _ns
                        sumc += _fc(j)(spci1)
                    Next
                    sumc *= rec
                    If _condtype <> Column.condtype.Full_Reflux Then
                        If _specs("C").SpecUnit = "% M/M" Then
                            spfval1 = _xc(0)(spci1) * _LSSj(0) - sumc
                        Else '% W/W
                            spfval1 = _pp.RET_VMM()(spci1) * 1000 * (_xc(0)(spci1) * _LSSj(0) - sumc)
                        End If
                    Else
                        If _specs("C").SpecUnit = "% M/M" Then
                            spfval1 = _yc(0)(spci1) * _Vj(0) - sumc
                        Else '% W/W
                            spfval1 = _pp.RET_VMM()(spci1) * 1000 * (_yc(0)(spci1) * _Vj(0) - sumc)
                        End If
                    End If
                Case ColumnSpec.SpecType.Heat_Duty
                    _Q(0) = spval1 / _maxF
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    If _condtype <> Column.condtype.Full_Reflux Then
                        spfval1 = _LSSj(0) - spval1 / _pp.AUX_MMM(_xc(0)) * 1000 / _maxF
                    Else
                        spfval1 = _Vj(0) - spval1 / _pp.AUX_MMM(_yc(0)) * 1000 / _maxF
                    End If
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                    If _condtype <> Column.condtype.Full_Reflux Then
                        spfval1 = _LSSj(0) - spval1 / _maxF
                    Else
                        spfval1 = _Vj(0) - spval1 / _maxF
                    End If
                Case ColumnSpec.SpecType.Stream_Ratio
                    If _condtype = Column.condtype.Total_Condenser Then
                        spfval1 = _Lj(0) - spval1 * _LSSj(0)
                    ElseIf _condtype = Column.condtype.Partial_Condenser Then
                        spfval1 = _Lj(0) - spval1 * (_LSSj(0) + _Vj(0))
                    Else
                        spfval1 = _Lj(0) - spval1 * _Vj(0)
                    End If
                Case ColumnSpec.SpecType.Temperature
                    spfval1 = _Tj(0) - spval1
            End Select

            'Reboiler Specs
            Select Case _specs("R").SType
                Case ColumnSpec.SpecType.Component_Fraction
                    If _specs("R").SpecUnit = "M" Then
                        spfval2 = _xc(_ns)(spci2) - spval2
                    Else 'W
                        spfval2 = _pp.AUX_CONVERT_MOL_TO_MASS(_xc(_ns))(spci2) - spval2
                    End If
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    spfval2 = _Lj(_ns) * _xc(_ns)(spci2) - spval2 / _pp.RET_VMM()(spci2) * 1000 / _maxF
                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                    spfval2 = _Lj(_ns) * _xc(_ns)(spci2) - spval2 / _maxF
                Case ColumnSpec.SpecType.Component_Recovery
                    Dim rec As Double = spval2 / 100
                    Dim sumc As Double = 0
                    For j = 0 To _ns
                        sumc += _fc(j)(spci2)
                    Next
                    sumc *= rec
                    If _specs("R").SpecUnit = "% M/M" Then
                        spfval2 = _lc(_ns)(spci2) - sumc
                    Else '% W/W
                        spfval2 = _pp.RET_VMM()(spci2) * 1000 * (_lc(_ns)(spci2) - sumc)
                    End If
                Case ColumnSpec.SpecType.Heat_Duty
                    _Q(_ns) = spval2 / _maxF
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    spfval2 = _Lj(_ns) - spval2 / _pp.AUX_MMM(_xc(_ns)) * 1000 / _maxF
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                    spfval2 = _Lj(_ns) - spval2 / _maxF
                Case ColumnSpec.SpecType.Stream_Ratio
                    spfval2 = _Vj(_ns) - spval2 * _Lj(_ns)
                Case ColumnSpec.SpecType.Temperature
                    spfval2 = _Tj(_ns) - spval2
            End Select

            'enthalpy balances

            Dim entbal(_ns) As Double

            For i = 0 To _ns
                If i = 0 Then
                    entbal(i) = (Hl(i) * _Rlj(i) * _Lj(i) + Hv(i) * _Rvj(i) * _Vj(i) - Hv(i + 1) * _Vj(i + 1) - _HF(i) * _F(i) - _Q(i))
                ElseIf i = _ns Then
                    entbal(i) = (Hl(i) * _Rlj(i) * _Lj(i) + Hv(i) * _Rvj(i) * _Vj(i) - Hl(i - 1) * _Lj(i - 1) - _HF(i) * _F(i) - _Q(i))
                Else
                    entbal(i) = (Hl(i) * _Rlj(i) * _Lj(i) + Hv(i) * _Rvj(i) * _Vj(i) - Hl(i - 1) * _Lj(i - 1) - Hv(i + 1) * _Vj(i + 1) - _HF(i) * _F(i) - _Q(i))
                End If
                entbal(i) /= (Hv(i) - Hl(i))
            Next

            Select Case _coltype
                Case Column.ColType.DistillationColumn
                    entbal(0) = spfval1 / spval1
                    entbal(_ns) = spfval2 / spval2
                Case Column.ColType.AbsorptionColumn
                    'do nothing
                Case Column.ColType.ReboiledAbsorber
                    entbal(_ns) = spfval2 / spval2
                Case Column.ColType.RefluxedAbsorber
                    entbal(0) = spfval1 / spval1
            End Select

            For i = 0 To x.Length - 1
                If i <= _ns Then
                    errors(i) = entbal(i)
                ElseIf i > _ns And i <= _vcnt + _ns Then
                    For j = 0 To _ns
                        If _Rvj(j) <> 1 Then
                            errors(i) = (_VSS(j) - _VSSj(j)) '/ _VSS(j)
                            i += 1
                        End If
                    Next
                End If
                If i > _vcnt + _ns And i <= _vcnt + _lcnt + _ns Then
                    For j = 1 To _ns
                        If _Rlj(j) <> 1 Then
                            errors(i) = (_LSS(j) - _LSSj(j)) '/ _LSS(j)
                            i += 1
                        End If
                    Next
                End If
            Next
            If _condtype = Column.condtype.Partial_Condenser Then errors(_el) = (_Vj(0) - Vjj(0))

            Return errors.AbsSqrSumY

        End Function

        Public Function FunctionGradient(ByVal x() As Double) As Double()

            Dim epsilon As Double = ndeps

            Dim f1, f2 As Double
            Dim g(x.Length - 1), x1(x.Length - 1), x2(x.Length - 1) As Double
            Dim j, k As Integer

            For j = 0 To x.Length - 1
                For k = 0 To x.Length - 1
                    x1(k) = x(k)
                    x2(k) = x(k)
                Next
                If x(j) <> 0.0# Then
                    x1(j) = x(j) * (1.0# + epsilon)
                    x2(j) = x(j) * (1.0# - epsilon)
                Else
                    x1(j) = x(j) + epsilon
                    x2(j) = x(j) - epsilon
                End If
                f1 = FunctionValue(x1)
                f2 = FunctionValue(x2)
                g(j) = (f2 - f1) / (x2(j) - x1(j))
            Next

            _pp.CurrentMaterialStream.Flowsheet.CheckStatus()

            Return g

        End Function

        Public Function Solve(ByVal nc As Integer, ByVal ns As Integer, ByVal maxits As Integer, _
                                ByVal tol As Array, ByVal F As Array, ByVal V As Array, _
                                ByVal Q As Array, ByVal L As Array, _
                                ByVal VSS As Array, ByVal LSS As Array, ByVal Kval()() As Double, _
                                ByVal x()() As Double, ByVal y()() As Double, ByVal z()() As Double, _
                                ByVal fc()() As Double, _
                                ByVal HF As Array, ByVal T As Array, ByVal P As Array, _
                                ByVal condt As DistillationColumn.condtype, _
                                ByVal eff() As Double, _
                                ByVal AdjustSb As Boolean,
                                ByVal coltype As Column.ColType, ByVal KbjWA As Boolean, _
                                ByVal pp As PropertyPackages.PropertyPackage, _
                                ByVal specs As Dictionary(Of String, SepOps.ColumnSpec), _
                                ByVal epsilon As Double, _
                                ByVal Solver As OptimizationMethod,
                                ByVal LowerBound As Double, ByVal UpperBound As Double,
                                ByVal IdealK As Boolean, ByVal IdealH As Boolean,
                                Optional ByVal llex As Boolean = False) As Object

            ik = IdealK
            ih = IdealH

            If ik Or ih Then
                _ppr = New PropertyPackages.RaoultPropertyPackage
                _ppr.CurrentMaterialStream = pp.CurrentMaterialStream
            End If

            Dim doparallel As Boolean = Settings.EnableParallelProcessing
            Dim poptions As New ParallelOptions() With {.MaxDegreeOfParallelism = Settings.MaxDegreeOfParallelism, .TaskScheduler = Settings.AppTaskScheduler}

            llextr = llex 'liq-liq extractor

            ndeps = epsilon

            Dim cv As New SystemsOfUnits.Converter
            Dim spval1, spval2 As Double
            Dim spci1, spci2 As Integer

            spval1 = SystemsOfUnits.Converter.ConvertToSI(specs("C").SpecUnit, specs("C").SpecValue)
            spci1 = specs("C").ComponentIndex
            spval2 = SystemsOfUnits.Converter.ConvertToSI(specs("R").SpecUnit, specs("R").SpecValue)
            spci2 = specs("R").ComponentIndex

            _ns = ns
            _nc = nc


            Dim ic, ec, iic As Integer
            Dim Tj(ns), Tj_ant(ns), T_(ns) As Double
            Dim Lj(ns), Vj(ns), xc(ns)(), yc(ns)(), lc(ns)(), vc(ns)(), zc(ns)() As Double
            Dim sum1(ns) As Double
            Dim i, j, w, m1, m2 As Integer

            'step0
            'normalize initial estimates

            Dim maxF As Double = MathEx.Common.Max(F)

            For i = 0 To ns
                F(i) = F(i) / maxF
                HF(i) = HF(i) / 1000
                L(i) = L(i) / maxF
                V(i) = V(i) / maxF
                LSS(i) = LSS(i) / maxF
                VSS(i) = VSS(i) / maxF
                Q(i) = Q(i) / maxF
            Next

            'step1

            Dim sumF As Double = 0
            Dim sumLSS As Double = 0
            Dim sumVSS As Double = 0
            For i = 0 To ns
                sumF += F(i)
                If i > 0 Then sumLSS += LSS(i)
                sumVSS += VSS(i)
            Next

            Dim B As Double
            If condt = Column.condtype.Total_Condenser Then
                B = sumF - sumLSS - sumVSS - LSS(0)
            ElseIf condt = Column.condtype.Partial_Condenser Then
                B = sumF - sumLSS - sumVSS - V(0) - LSS(0)
            Else
                B = sumF - sumLSS - sumVSS - V(0)
            End If

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
                If LSS(i) <> 0 And i > 0 Then
                    el += 1
                    lsi.Add(i)
                End If
            Next
            If condt = Column.condtype.Partial_Condenser Then el += 1

            Dim hes(el, el) As Double
            Dim bx(el), bxb(el), bf(el), bfb(el), bp(el), bp_ant(el) As Double

            Dim f1(el), f2(el), f3(el), f4(el) As Double
            Dim u As Integer = 0

            'find Kbref 

            Dim Kbj(ns), Kbj_ant(ns) As Object
            Dim K(ns, nc - 1), K_ant(ns, nc - 1), K2j(ns, nc - 1) As Object

            Dim Kw1(ns)(), Kw2(ns)() As Object
            Dim wi(ns, nc - 1), ti(ns, nc - 1), sumwi(ns), sumti(ns) As Double
            For i = 0 To ns
                Array.Resize(Kw1(i), nc)
                Array.Resize(Kw2(i), nc)
            Next

            Dim tmp0 As Object = Nothing

            For i = 0 To ns
                If ik Then
                    If Not llextr Then tmp0 = _ppr.DW_CalcKvalue(z(i), T(i), P(i))
                Else
                    If Not llextr Then tmp0 = pp.DW_CalcKvalue(z(i), T(i), P(i))
                End If
                For j = 0 To nc - 1
                    If Not llextr Then K(i, j) = tmp0(j) Else K(i, j) = Kval(i)(j)
                    If Double.IsNaN(K(i, j)) Or Double.IsInfinity(K(i, j)) Or K(i, j) = 0 Then K(i, j) = pp.AUX_PVAPi(j, T(i)) / P(i)
                Next
                pp.CurrentMaterialStream.Flowsheet.CheckStatus()
            Next

            If KbjWA = False Then
                Kbj = CalcKbj1(ns, nc, K, z, y, T, P, pp)
            Else
                Kbj = CalcKbj2(ns, nc, K, z, y, T, P, pp)
            End If

            'relative volatilities

            Dim alpha(ns, nc - 1), alpha_ant(ns, nc - 1) As Double

            For i = 0 To ns
                For j = 0 To nc - 1
                    alpha(i, j) = K(i, j) / Kbj(i)
                Next
            Next

            'initialize A/B/C/D/E/F

            Dim Kbj1(ns), Kbj2(ns) As Object
            Dim Tj1(ns), Tj2(ns), Aj(ns), Bj(ns), Cj(ns), Dj(ns), Ej(ns), Fj(ns) As Double
            Dim Aj_ant(ns), Bj_ant(ns), Cj_ant(ns), Dj_ant(ns), Ej_ant(ns), Fj_ant(ns) As Double
            Dim Hl1(ns), Hl2(ns), Hv1(ns), Hv2(ns) As Double
            Dim K2(ns)() As Double
            Dim Hv(ns), Hl(ns), DHv(ns), DHl(ns), Hidv(ns), Hidl(ns) As Double

            For i = 0 To ns
                T_(i) = T(i) - 1
                Tj(i) = T(i)
                'Kbjs, Ts
                Tj1(i) = T(i)
                Tj2(i) = T(i) + 1
                Kbj1(i) = Kbj(i)
                'new Ks
                If ih Then
                    If llextr Then
                        K2(i) = _ppr.DW_CalcKvalue(x(i), y(i), Tj2(i), P(i), "LL")
                        Hv1(i) = _ppr.DW_CalcEnthalpyDeparture(y(i), Tj1(i), P(i), PropertyPackages.State.Liquid)
                        Hv2(i) = _ppr.DW_CalcEnthalpyDeparture(y(i), Tj2(i), P(i), PropertyPackages.State.Liquid)
                    Else
                        K2(i) = _ppr.DW_CalcKvalue(x(i), y(i), Tj2(i), P(i))
                        Hv1(i) = _ppr.DW_CalcEnthalpyDeparture(y(i), Tj1(i), P(i), PropertyPackages.State.Vapor)
                        Hv2(i) = _ppr.DW_CalcEnthalpyDeparture(y(i), Tj2(i), P(i), PropertyPackages.State.Vapor)
                    End If
                    Hl1(i) = _ppr.DW_CalcEnthalpyDeparture(x(i), Tj1(i), P(i), PropertyPackages.State.Liquid)
                    Hl2(i) = _ppr.DW_CalcEnthalpyDeparture(x(i), Tj2(i), P(i), PropertyPackages.State.Liquid)
                Else
                    If llextr Then
                        K2(i) = pp.DW_CalcKvalue(x(i), y(i), Tj2(i), P(i), "LL")
                        Hv1(i) = pp.DW_CalcEnthalpyDeparture(y(i), Tj1(i), P(i), PropertyPackages.State.Liquid)
                        Hv2(i) = pp.DW_CalcEnthalpyDeparture(y(i), Tj2(i), P(i), PropertyPackages.State.Liquid)
                    Else
                        K2(i) = pp.DW_CalcKvalue(x(i), y(i), Tj2(i), P(i))
                        Hv1(i) = pp.DW_CalcEnthalpyDeparture(y(i), Tj1(i), P(i), PropertyPackages.State.Vapor)
                        Hv2(i) = pp.DW_CalcEnthalpyDeparture(y(i), Tj2(i), P(i), PropertyPackages.State.Vapor)
                    End If
                    Hl1(i) = pp.DW_CalcEnthalpyDeparture(x(i), Tj1(i), P(i), PropertyPackages.State.Liquid)
                    Hl2(i) = pp.DW_CalcEnthalpyDeparture(x(i), Tj2(i), P(i), PropertyPackages.State.Liquid)
                End If
                For j = 0 To nc - 1
                    K2j(i, j) = K2(i)(j)
                    If Double.IsNaN(K2(i)(j)) Or Double.IsInfinity(K2(i)(j)) Then K2(i)(j) = pp.AUX_PVAPi(j, T(i)) / P(i)
                Next
            Next

            If KbjWA = False Then
                Kbj2 = CalcKbj1(ns, nc, K2j, z, y, Tj2, P, pp)
            Else
                Kbj2 = CalcKbj2(ns, nc, K2j, z, y, Tj2, P, pp)
            End If

            For i = 0 To ns
                Bj(i) = Log(Kbj1(i) / Kbj2(i)) / (1 / Tj2(i) - 1 / Tj1(i))
                Aj(i) = Log(Kbj1(i)) + Bj(i) * (1 / Tj1(i))
                Dj(i) = (Hv1(i) - Hv2(i)) / (Tj1(i) - Tj2(i))
                Cj(i) = Hv1(i) - Dj(i) * (Tj1(i) - T_(i))
                Fj(i) = (Hl1(i) - Hl2(i)) / (Tj1(i) - Tj2(i))
                Ej(i) = Hl1(i) - Fj(i) * (Tj1(i) - T_(i))
            Next

            'external loop

            Dim Sb, sbf, sbf_ant, sbf_ant2, sbx, sbx_ant, sbx_ant2 As Double
            Dim SbOK As Boolean = True
            Dim BuildingJacobian As Boolean = False

            If AdjustSb Then SbOK = False

            Sb = 1

            Dim el_err As Double = 0.0#
            Dim el_err_ant As Double = 0.0#
            Dim il_err As Double = 0.0#
            Dim il_err_ant As Double = 0.0#

            'independent variables -> stripping and withdrawal factors

            Dim Sbj(ns), lnSbj0(ns), lnSbj(ns), S(ns, nc - 1) As Double
            Dim Rvj(ns), Rlj(ns), lnRvj(ns), lnRlj(ns), lnRvj0(ns), lnRlj0(ns) As Double
            Dim VSSj(ns), LSSj(ns), PSbj As Double
            Dim Nss As Integer = ns + 1
            'Calculo de Sbj, Rlj y Rvj del Lazo Externo

            For i = 0 To ns
                Sbj(i) = Kbj(i) * V(i) / L(i)
            Next
            If AdjustSb Then
                SbOK = False
                PSbj = 1
                For i = 0 To ns
                    If i = 0 And condt = Column.condtype.Total_Condenser Then
                        Nss -= 1
                    Else
                        PSbj *= Sbj(i)
                    End If
                Next
                Sb = PSbj ^ (1 / (Nss))
            Else
                Sb = 1
            End If

            For i = 0 To ns
                If Sbj(i) = 0 Then Sbj(i) = 1.0E-20
                lnSbj(i) = Log(Sbj(i))
                If V(i) <> 0 Then Rvj(i) = 1 + VSS(i) / V(i) Else Rvj(i) = 1
                lnRvj(i) = Log(Rvj(i))
                If L(i) <> 0 Then Rlj(i) = 1 + LSS(i) / L(i) Else Rlj(i) = 1
                lnRlj(i) = Log(Rlj(i))
                For j = 0 To nc - 1
                    S(i, j) = Sbj(i) * alpha(i, j) * Sb
                Next
            Next

            Dim vcnt, lcnt As Integer

            vcnt = 0
            For i = 0 To ns
                If lnRvj(i) <> 0 And Not Double.IsInfinity(lnRvj(i)) Then
                    vcnt += 1
                End If
            Next

            lcnt = 0
            For i = 1 To ns
                If lnRlj(i) <> 0 And Not Double.IsInfinity(lnRlj(i)) Then
                    lcnt += 1
                End If
            Next

            'internal loop

            Dim check1 As Boolean = False
            Dim num, denom, x0, fx0 As New ArrayList

            w = 0

1:          Dim ic0 As Integer = 0

            Do

                If Not SbOK Then

                    sbf_ant2 = sbf_ant
                    sbf_ant = sbf

                    sumF = 0
                    sumLSS = 0
                    sumVSS = 0
                    For j = 0 To _ns
                        sumF += F(j)
                        If j > 0 Then sumLSS += LSS(j)
                        sumVSS += VSS(j)
                    Next

                    sbf = sumF - sumLSS - sumVSS - V(0) - L(ns) - LSS(0)

                    sbx_ant2 = sbx_ant
                    sbx_ant = sbx

                    If ic0 > 1 Then
                        If Abs((-sbf * (sbx - sbx_ant2) / (sbf - sbf_ant2)) / sbx) > 1 Then
                            sbx = sbx_ant2 * 1.01
                        Else
                            sbx = sbx - sbf * (sbx - sbx_ant2) / (sbf - sbf_ant2)
                        End If
                    Else
                        sbx = Sb * 1.01
                    End If

                    If sbx < 0 Then sbx = Abs(sbx)

                    Sb = sbx

                    For i = 0 To ns
                        Sbj(i) = Kbj(i) * V(i) / L(i)
                        If Sbj(i) = 0 Then Sbj(i) = 1.0E-20
                        lnSbj(i) = Log(Sbj(i))
                        If V(i) <> 0 Then Rvj(i) = 1 + VSS(i) / V(i) Else Rvj(i) = 1
                        lnRvj(i) = Log(Rvj(i))
                        If L(i) <> 0 Then Rlj(i) = 1 + LSS(i) / L(i) Else Rlj(i) = 1
                        lnRlj(i) = Log(Rlj(i))
                        For j = 0 To nc - 1
                            S(i, j) = Sbj(i) * alpha(i, j) * Sb
                        Next
                    Next
                End If

                If SbOK Then Exit Do

                If sbx > 10 Then Sb = sbx_ant2

                ic0 += 1

                pp.CurrentMaterialStream.Flowsheet.CheckStatus()

                If Double.IsNaN(sbf) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCSbError"))

            Loop Until Abs(sbf) < 0.001

            SbOK = True

            Dim fx(el), dfdx(el, el), dfdx_ant(el, el), dx(el), xvar(el), xvar_ant(el) As Double
            Dim jac As New Mapack.Matrix(el + 1, el + 1), hesm As New Mapack.Matrix(el + 1, el + 1)
            Dim perturb As Boolean = False, bypass As Boolean = False

            ec = 0
            ic = 0
            Do

                iic = 0

                'step3


                For i = 0 To ns
                    'store initial values
                    lnSbj0(i) = lnSbj(i)
                    lnRvj0(i) = lnRvj(i)
                    lnRlj0(i) = lnRlj(i)
                Next

                'update inner loop parameters

                Dim lnSbj_ant(ns), lnRvj_ant(ns), lnRlj_ant(ns), df, df_ant As Double

                df_ant = df

                _Bj = Bj.Clone
                _Aj = Aj.Clone
                _Cj = Cj.Clone
                _Dj = Dj.Clone
                _Ej = Ej.Clone
                _Fj = Fj.Clone
                _eff = eff.Clone
                _Tj = Tj.Clone
                _T_ = T_.Clone
                _Lj = Lj.Clone
                _Vj = Vj.Clone
                Vjj = V.Clone
                _LSSj = LSS.Clone
                _VSSj = VSS.Clone
                _LSS = LSS.Clone
                _VSS = VSS.Clone
                _Rlj = Rlj.Clone
                _Rvj = Rvj.Clone
                _F = F.Clone
                _P = P.Clone
                _HF = HF.Clone
                _Q = Q.Clone
                _S = S.Clone
                _condtype = condt
                _alpha = alpha.Clone
                _fc = fc.Clone
                _xc = xc.Clone
                _yc = yc.Clone
                _lc = lc.Clone
                _vc = vc.Clone
                _zc = zc.Clone
                _Kbj = Kbj.Clone
                _pp = pp
                _coltype = coltype
                _bx = bx.Clone
                _dbx = bp.Clone
                _Sb = Sb
                _vcnt = vcnt
                _lcnt = lcnt
                _specs = specs
                _maxF = maxF
                _el = el

                'solve using newton's method

                For i = 0 To ns
                    If i = 0 And condt <> Column.condtype.Full_Reflux Then
                        xvar(i) = lnRlj(i)
                    Else
                        xvar(i) = lnSbj(i)
                    End If
                Next

                m1 = 0

                If vcnt > 0 Then
                    For i = ns + 1 To vcnt + ns
                        For j = m1 To ns
                            If Rvj(j) <> 1 Then
                                m1 = j + 1
                                Exit For
                            End If
                        Next
                        xvar(i) = lnRvj(m1 - 1)
                    Next
                End If

                m2 = 0

                If lcnt > 0 Then
                    For i = vcnt + ns + 1 To vcnt + lcnt + ns
                        For j = m2 + 1 To ns
                            If Rlj(j) <> 1 Then
                                m2 = j + 1
                                Exit For
                            End If
                        Next
                        xvar(i) = lnRlj(m2 - 1)
                    Next
                End If
                If condt = Column.condtype.Partial_Condenser Then xvar(el) = lnSbj(0)

                ic0 = 0

                'solve inner loop

                Dim obj As Double = 0.0#
                Dim status As IpoptReturnCode = IpoptReturnCode.Feasible_Point_Found

                Dim initval(xvar.Length - 1) As Double
                Dim lconstr(xvar.Length - 1) As Double
                Dim uconstr(xvar.Length - 1) As Double

                For i = 0 To xvar.Length - 1
                    initval(i) = xvar(i)
                    lconstr(i) = LowerBound
                    uconstr(i) = UpperBound
                Next

                Dim n As Integer = xvar.Length - 1

                Select Case Solver
                    Case OptimizationMethod.Limited_Memory_BGFS
                        Dim variables(n) As OptBoundVariable
                        For i = 0 To n
                            variables(i) = New OptBoundVariable("x" & CStr(i + 1), initval(i))
                        Next
                        Dim _solver As New L_BFGS_B
                        _solver.Tolerance = tol(1)
                        _solver.MaxFunEvaluations = maxits
                        initval = _solver.ComputeMin(AddressOf FunctionValue, AddressOf FunctionGradient, variables)
                        _solver = Nothing
                    Case OptimizationMethod.Truncated_Newton
                        Dim variables(n) As OptBoundVariable
                        For i = 0 To n
                            variables(i) = New OptBoundVariable("x" & CStr(i + 1), initval(i))
                        Next
                        Dim _solver As New TruncatedNewton
                        _solver.Tolerance = tol(1)
                        _solver.MaxFunEvaluations = maxits
                        initval = _solver.ComputeMin(AddressOf FunctionValue, AddressOf FunctionGradient, variables)
                        _solver = Nothing
                    Case OptimizationMethod.Simplex
                        Dim variables(n) As OptBoundVariable
                        For i = 0 To n
                            variables(i) = New OptBoundVariable("x" & CStr(i + 1), initval(i))
                        Next
                        Dim _solver As New Simplex
                        _solver.Tolerance = tol(1)
                        _solver.MaxFunEvaluations = maxits
                        initval = _solver.ComputeMin(AddressOf FunctionValue, variables)
                        _solver = Nothing
                    Case OptimizationMethod.IPOPT
                        Calculator.CheckParallelPInvoke()
                        Using problem As New Ipopt(xvar.Length, lconstr, uconstr, 0, Nothing, Nothing,
                        0, 0, AddressOf eval_f, AddressOf eval_g,
                        AddressOf eval_grad_f, AddressOf eval_jac_g, AddressOf eval_h)
                            problem.AddOption("tol", tol(1))
                            problem.AddOption("max_iter", maxits)
                            problem.AddOption("mu_strategy", "adaptive")
                            problem.AddOption("hessian_approximation", "limited-memory")
                            problem.SetIntermediateCallback(AddressOf intermediate)
                            status = problem.SolveProblem(initval, obj, Nothing, Nothing, Nothing, Nothing)
                        End Using
                    Case OptimizationMethod.DifferentialEvolution, OptimizationMethod.GradientDescent, OptimizationMethod.LocalUnimodalSampling,
                        OptimizationMethod.ManyOptimizingLiaisons, OptimizationMethod.Mesh, OptimizationMethod.ParticleSwarm, OptimizationMethod.ParticleSwarmOptimization

                        SwarmOps.Globals.Random = New RandomOps.MersenneTwister()

                        Dim sproblem As New Russell_ColumnProblem(Me) With {._Dim = initval.Length, ._LB = lconstr, ._UB = uconstr, ._INIT = initval, ._Name = "IO"}
                        sproblem.MaxIterations = maxits
                        sproblem.MinIterations = maxits / 2
                        sproblem.Tolerance = tol(1)
                        sproblem.RequireFeasible = True
                        Dim opt As SwarmOps.Optimizer = GetSolver(Solver)
                        opt.Problem = sproblem
                        Dim sresult = opt.Optimize(opt.DefaultParameters)

                        initval = sresult.Parameters

                End Select

                xvar = initval

                il_err = FunctionValue(xvar)

                pp.CurrentMaterialStream.Flowsheet.ShowMessage("Inside-Out solver: inner loop error value = " & il_err, IFlowsheet.MessageType.Information)

                For i = 0 To ns
                    If i = 0 And _condtype <> Column.condtype.Full_Reflux Then
                        lnRlj_ant(i) = lnRlj(i)
                        lnRlj(i) = xvar(i)
                        Rlj(i) = Exp(lnRlj(i))
                    Else
                        lnSbj_ant(i) = lnSbj(i)
                        lnSbj(i) = xvar(i)
                        Sbj(i) = Exp(lnSbj(i))
                        For j = 0 To nc - 1
                            S(i, j) = Sbj(i) * alpha(i, j) * Sb
                        Next
                    End If
                Next

                m1 = 0

                If vcnt > 0 Then
                    For i = ns To vcnt + ns
                        For j = m1 To ns
                            If Rvj(j) <> 1 Then
                                m1 = j + 1
                                Exit For
                            End If
                        Next
                        lnRvj_ant(m1 - 1) = lnRvj(m1 - 1)
                        lnRvj(m1 - 1) = xvar(i)
                        Rvj(m1 - 1) = Exp(lnRvj(m1 - 1))
                    Next
                End If

                m2 = 0

                If lcnt > 0 Then
                    For i = vcnt + ns + 1 To vcnt + lcnt + ns
                        For j = m2 + 1 To ns
                            If Rlj(j) <> 1 Then
                                m2 = j + 1
                                Exit For
                            End If
                        Next
                        lnRlj_ant(m2 - 1) = lnRlj(m2 - 1)
                        lnRlj(m2 - 1) = xvar(i)
                        Rlj(m2 - 1) = Exp(lnRlj(m2 - 1))
                    Next
                End If
                If condt = Column.condtype.Partial_Condenser Then
                    lnSbj_ant(0) = lnSbj(0)
                    lnSbj(0) = xvar(el)
                    Sbj(0) = Exp(lnSbj(0))
                    For j = 0 To nc - 1
                        S(0, j) = Sbj(0) * alpha(0, j) * Sb
                    Next
                End If
                ic += 1
                iic += 1

                'step9 (external loop)

                Tj_ant = Tj.Clone
                Tj = _Tj.Clone
                T_ = _T_.Clone
                Lj = _Lj.Clone
                Vj = _Vj.Clone
                Q = _Q.Clone
                LSSj = _LSSj.Clone
                VSSj = _VSSj.Clone
                xc = _xc.Clone
                yc = _yc.Clone
                lc = _lc.Clone
                vc = _vc.Clone
                zc = _zc.Clone
                Kbj = _Kbj.Clone

                For i = 0 To ns

                    T_(i) = Tj(i)
                    Tj1(i) = Tj(i)
                    Tj2(i) = Tj(i) + 0.1

                Next

                'update external loop variables using rigorous models

                el_err_ant = el_err
                el_err = 0

                Dim tmp(ns) As Object

                If doparallel Then
                    If Settings.EnableGPUProcessing Then
                        'Settings.gpu.EnableMultithreading()
                    End If

                    Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, ns + 1, poptions,
                                                             Sub(ipar)
                                                                 If ik Then
                                                                     If llextr Then
                                                                         tmp(ipar) = _ppr.DW_CalcKvalue(xc(ipar), yc(ipar), Tj(ipar), P(ipar), "LL")
                                                                     Else
                                                                         tmp(ipar) = _ppr.DW_CalcKvalue(xc(ipar), yc(ipar), Tj(ipar), P(ipar))
                                                                     End If
                                                                 Else
                                                                     If llextr Then
                                                                         tmp(ipar) = pp.DW_CalcKvalue(xc(ipar), yc(ipar), Tj(ipar), P(ipar), "LL")
                                                                     Else
                                                                         tmp(ipar) = pp.DW_CalcKvalue(xc(ipar), yc(ipar), Tj(ipar), P(ipar))
                                                                     End If
                                                                 End If
                                                             End Sub),
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                      Settings.AppTaskScheduler)
                    task1.Wait()
                    For i = 0 To ns
                        For j = 0 To nc - 1
                            K_ant(i, j) = K(i, j)
                            K(i, j) = tmp(i)(j)
                        Next
                        Kbj_ant(i) = Kbj(i)
                    Next

                Else
                    For i = 0 To ns
                        If ik Then
                            If llextr Then
                                tmp(i) = _ppr.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i), "LL")
                            Else
                                tmp(i) = _ppr.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i))
                            End If
                        Else
                            If llextr Then
                                tmp(i) = pp.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i), "LL")
                            Else
                                tmp(i) = pp.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i))
                            End If
                        End If
                        For j = 0 To nc - 1
                            K_ant(i, j) = K(i, j)
                            K(i, j) = tmp(i)(j)
                        Next
                        Kbj_ant(i) = Kbj(i)
                    Next
                End If

                If KbjWA = False Then
                    Kbj1 = CalcKbj1(ns, nc, K, zc, yc, Tj1, P, pp)
                Else
                    Kbj1 = CalcKbj2(ns, nc, K, zc, yc, Tj1, P, pp)
                End If
                Kbj = Kbj1

                'update relative volatilities

                For i = 0 To ns
                    For j = 0 To nc - 1
                        alpha_ant(i, j) = alpha(i, j)
                        alpha(i, j) = K(i, j) / Kbj(i)
                        el_err += Abs((alpha(i, j) - alpha_ant(i, j)) / alpha_ant(i, j)) ^ 2
                    Next
                Next

                For i = 0 To ns
                    Sbj(i) = Kbj(i) * Vj(i) / Lj(i)
                    If Sbj(i) = 0.0# Then Sbj(i) = 1.0E-20
                    lnSbj(i) = Log(Sbj(i))
                    If Vj(i) <> 0 Then Rvj(i) = 1 + VSSj(i) / Vj(i) Else Rvj(i) = 1
                    lnRvj(i) = Log(Rvj(i))
                    If Lj(i) <> 0 Then Rlj(i) = 1 + LSSj(i) / Lj(i) Else Rlj(i) = 1
                    lnRlj(i) = Log(Rlj(i))
                    For j = 0 To nc - 1
                        S(i, j) = Sbj(i) * alpha(i, j) * Sb
                    Next
                Next

                'update A/B/C/D/E/F

                If doparallel Then

                    Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, ns + 1, poptions,
                                                             Sub(ipar)
                                                                 'new Ks
                                                                 If ik Then
                                                                     K2(ipar) = _ppr.DW_CalcKvalue(xc(ipar), yc(ipar), Tj2(ipar), P(ipar))
                                                                 Else
                                                                     K2(ipar) = pp.DW_CalcKvalue(xc(ipar), yc(ipar), Tj2(ipar), P(ipar))
                                                                 End If
                                                             End Sub),
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                      Settings.AppTaskScheduler)
                    task1.Wait()
                    For i = 0 To ns
                        For j = 0 To nc - 1
                            K2j(i, j) = K2(i)(j)
                            If Double.IsNaN(K2(i)(j)) Or Double.IsInfinity(K2(i)(j)) Then K2(i)(j) = pp.AUX_PVAPi(j, Tj(i)) / P(i)
                        Next
                    Next
                Else
                    For i = 0 To ns

                        'new Ks
                        If ik Then
                            K2(i) = _ppr.DW_CalcKvalue(xc(i), yc(i), Tj2(i), P(i))
                        Else
                            K2(i) = pp.DW_CalcKvalue(xc(i), yc(i), Tj2(i), P(i))
                        End If

                        For j = 0 To nc - 1
                            K2j(i, j) = K2(i)(j)
                            If Double.IsNaN(K2(i)(j)) Or Double.IsInfinity(K2(i)(j)) Then K2(i)(j) = pp.AUX_PVAPi(j, Tj(i)) / P(i)
                        Next

                    Next
                End If

                If doparallel Then

                    Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, ns + 1, poptions,
                                                              Sub(ipar)
                                                                  'enthalpies
                                                                  If ih Then
                                                                      If llextr Then
                                                                          Hv1(ipar) = _ppr.DW_CalcEnthalpyDeparture(yc(ipar), Tj1(ipar), P(ipar), PropertyPackages.State.Liquid)
                                                                          Hv2(ipar) = _ppr.DW_CalcEnthalpyDeparture(yc(ipar), Tj2(ipar), P(ipar), PropertyPackages.State.Liquid)
                                                                      Else
                                                                          Hv1(ipar) = _ppr.DW_CalcEnthalpyDeparture(yc(ipar), Tj1(ipar), P(ipar), PropertyPackages.State.Vapor)
                                                                          Hv2(ipar) = _ppr.DW_CalcEnthalpyDeparture(yc(ipar), Tj2(ipar), P(ipar), PropertyPackages.State.Vapor)
                                                                      End If
                                                                      Hl1(ipar) = _ppr.DW_CalcEnthalpyDeparture(xc(ipar), Tj1(ipar), P(ipar), PropertyPackages.State.Liquid)
                                                                      Hl2(ipar) = _ppr.DW_CalcEnthalpyDeparture(xc(ipar), Tj2(ipar), P(ipar), PropertyPackages.State.Liquid)
                                                                  Else
                                                                      If llextr Then
                                                                          Hv1(ipar) = pp.DW_CalcEnthalpyDeparture(yc(ipar), Tj1(ipar), P(ipar), PropertyPackages.State.Liquid)
                                                                          Hv2(ipar) = pp.DW_CalcEnthalpyDeparture(yc(ipar), Tj2(ipar), P(ipar), PropertyPackages.State.Liquid)
                                                                      Else
                                                                          Hv1(ipar) = pp.DW_CalcEnthalpyDeparture(yc(ipar), Tj1(ipar), P(ipar), PropertyPackages.State.Vapor)
                                                                          Hv2(ipar) = pp.DW_CalcEnthalpyDeparture(yc(ipar), Tj2(ipar), P(ipar), PropertyPackages.State.Vapor)
                                                                      End If
                                                                      Hl1(ipar) = pp.DW_CalcEnthalpyDeparture(xc(ipar), Tj1(ipar), P(ipar), PropertyPackages.State.Liquid)
                                                                      Hl2(ipar) = pp.DW_CalcEnthalpyDeparture(xc(ipar), Tj2(ipar), P(ipar), PropertyPackages.State.Liquid)
                                                                  End If
                                                              End Sub),
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                      Settings.AppTaskScheduler)
                    task1.Wait()

                Else
                    For i = 0 To ns

                        'enthalpies
                        If ih Then
                            If llextr Then
                                Hv1(i) = _ppr.DW_CalcEnthalpyDeparture(yc(i), Tj1(i), P(i), PropertyPackages.State.Liquid)
                                Hv2(i) = _ppr.DW_CalcEnthalpyDeparture(yc(i), Tj2(i), P(i), PropertyPackages.State.Liquid)
                            Else
                                Hv1(i) = _ppr.DW_CalcEnthalpyDeparture(yc(i), Tj1(i), P(i), PropertyPackages.State.Vapor)
                                Hv2(i) = _ppr.DW_CalcEnthalpyDeparture(yc(i), Tj2(i), P(i), PropertyPackages.State.Vapor)
                            End If
                            Hl1(i) = _ppr.DW_CalcEnthalpyDeparture(xc(i), Tj1(i), P(i), PropertyPackages.State.Liquid)
                            Hl2(i) = _ppr.DW_CalcEnthalpyDeparture(xc(i), Tj2(i), P(i), PropertyPackages.State.Liquid)
                        Else
                            If llextr Then
                                Hv1(i) = pp.DW_CalcEnthalpyDeparture(yc(i), Tj1(i), P(i), PropertyPackages.State.Liquid)
                                Hv2(i) = pp.DW_CalcEnthalpyDeparture(yc(i), Tj2(i), P(i), PropertyPackages.State.Liquid)
                            Else
                                Hv1(i) = pp.DW_CalcEnthalpyDeparture(yc(i), Tj1(i), P(i), PropertyPackages.State.Vapor)
                                Hv2(i) = pp.DW_CalcEnthalpyDeparture(yc(i), Tj2(i), P(i), PropertyPackages.State.Vapor)
                            End If
                            Hl1(i) = pp.DW_CalcEnthalpyDeparture(xc(i), Tj1(i), P(i), PropertyPackages.State.Liquid)
                            Hl2(i) = pp.DW_CalcEnthalpyDeparture(xc(i), Tj2(i), P(i), PropertyPackages.State.Liquid)
                        End If

                    Next
                End If

                If KbjWA = False Then
                    Kbj2 = CalcKbj1(ns, nc, K2j, zc, yc, Tj2, P, pp)
                Else
                    Kbj2 = CalcKbj2(ns, nc, K2j, zc, yc, Tj2, P, pp)
                End If

                Dim Aerr(ns), Berr(ns) As Double

                For i = 0 To ns
                    Bj_ant(i) = Bj(i)
                    Bj(i) = Log(Kbj1(i) / Kbj2(i)) / (1 / Tj2(i) - 1 / Tj1(i))
                    Berr(i) = Bj(i) - Bj_ant(i)
                    Aj_ant(i) = Aj(i)
                    Aj(i) = Log(Kbj1(i)) + Bj(i) * (1 / Tj1(i))
                    Aerr(i) = Aj(i) - Aj_ant(i)
                    Dj_ant(i) = Dj(i)
                    Dj(i) = (Hv1(i) - Hv2(i)) / (Tj1(i) - Tj2(i))
                    Cj_ant(i) = Cj(i)
                    Cj(i) = Hv1(i) - Dj(i) * (Tj1(i) - T_(i))
                    Fj_ant(i) = Fj(i)
                    Fj(i) = (Hl1(i) - Hl2(i)) / (Tj1(i) - Tj2(i))
                    Ej_ant(i) = Ej(i)
                    Ej(i) = Hl1(i) - Fj(i) * (Tj1(i) - T_(i))
                Next

                ec += 1

                If ec >= maxits And Not IdealK And Not IdealH Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCMaxIterationsReached"))
                If Double.IsNaN(el_err) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))

                If AdjustSb Then SbOK = False
                Sb = 1

                pp.CurrentMaterialStream.Flowsheet.CheckStatus()

                pp.CurrentMaterialStream.Flowsheet.ShowMessage("Inside-Out solver: outer loop error value = " & el_err, IFlowsheet.MessageType.Information)

            Loop Until Abs(el_err) < tol(1)

            il_err = FunctionValue(xvar)

            If Abs(il_err) > tol(0) And Not IdealK And Not IdealH Then
                Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCErrorStillHigh"))
            End If

            ' finished, de-normalize and return arrays

            For i = 0 To ns
                Lj(i) = Lj(i) * maxF
                Vj(i) = Vj(i) * maxF
                LSSj(i) = LSSj(i) * maxF
                VSSj(i) = VSSj(i) * maxF
                F(i) = F(i) * maxF
                L(i) = L(i) * maxF
                V(i) = V(i) * maxF
                LSS(i) = LSS(i) * maxF
                VSS(i) = VSS(i) * maxF
                Q(i) = Q(i) * maxF
            Next

            Return New Object() {Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ic, il_err, ec, el_err, dfdx}

        End Function

        Public Function eval_f(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByRef obj_value As Double) As Boolean
            Dim fval As Double = FunctionValue(x)
            obj_value = fval
            Return True
        End Function

        Public Function eval_grad_f(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByRef grad_f As Double()) As Boolean
            Dim g As Double() = FunctionGradient(x)
            grad_f = g
            Return True
        End Function

        Public Function eval_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByRef g As Double()) As Boolean
            Return True
        End Function

        Public Function eval_jac_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByVal nele_jac As Integer, ByRef iRow As Integer(), _
         ByRef jCol As Integer(), ByRef values As Double()) As Boolean
            Return False
        End Function

        Public Function eval_h(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal obj_factor As Double, ByVal m As Integer, ByVal lambda As Double(), _
         ByVal new_lambda As Boolean, ByVal nele_hess As Integer, ByRef iRow As Integer(), ByRef jCol As Integer(), ByRef values As Double()) As Boolean
            Return False
        End Function

        Public Function intermediate(ByVal alg_mod As IpoptAlgorithmMode, ByVal iter_count As Integer, ByVal obj_value As Double,
                             ByVal inf_pr As Double, ByVal inf_du As Double, ByVal mu As Double,
                             ByVal d_norm As Double, ByVal regularization_size As Double, ByVal alpha_du As Double,
                             ByVal alpha_pr As Double, ByVal ls_trials As Integer) As Boolean
            '_pp.CurrentMaterialStream.Flowsheet.ShowMessage("Inside-Out solver iteration #" & iter_count & ": current objective function (error) value = " & obj_value, IFlowsheet.MessageType.Information)
            Return True
        End Function

    End Class

    <System.Serializable()> Public Class WangHenkeMethod

        Public Shared Function Solve(ByVal nc As Integer, ByVal ns As Integer, ByVal maxits As Integer, _
                                ByVal tol As Array, ByVal F As Array, ByVal V As Array, _
                                ByVal Q As Array, ByVal L As Array, _
                                ByVal VSS As Array, ByVal LSS As Array, ByVal Kval()() As Double, _
                                ByVal x()() As Double, ByVal y()() As Double, ByVal z()() As Double, _
                                ByVal fc()() As Double, _
                                ByVal HF As Array, ByVal T As Array, ByVal P As Array, _
                                ByVal condt As DistillationColumn.condtype, _
                                ByVal stopatitnumber As Integer, _
                                ByVal eff() As Double, _
                                ByVal coltype As Column.ColType, _
                                ByVal pp As PropertyPackages.PropertyPackage, _
                                ByVal specs As Dictionary(Of String, SepOps.ColumnSpec),
                                ByVal IdealK As Boolean, ByVal IdealH As Boolean) As Object

            Dim ppr As PropertyPackages.RaoultPropertyPackage = Nothing

            If IdealK Or IdealH Then
                ppr = New PropertyPackages.RaoultPropertyPackage
                ppr.CurrentMaterialStream = pp.CurrentMaterialStream
            End If

            Dim doparallel As Boolean = Settings.EnableParallelProcessing
            Dim poptions As New ParallelOptions() With {.MaxDegreeOfParallelism = Settings.MaxDegreeOfParallelism, .TaskScheduler = Settings.AppTaskScheduler}

            Dim ic As Integer
            Dim t_error, t_error_ant As Double
            Dim Tj(ns), Tj_ant(ns), dTj(ns) As Double
            Dim Fj(ns), Lj(ns), Vj(ns), Vj_ant(ns), dVj(ns), xc(ns)(), fcj(ns)(), yc(ns)(), lc(ns)(), vc(ns)(), zc(ns)(), K(ns)() As Double
            Dim Hfj(ns), Hv(ns), Hl(ns) As Double
            Dim VSSj(ns), LSSj(ns) As Double

            'step0

            Dim cv As New SystemsOfUnits.Converter
            Dim spval1, spval2 As Double

            spval1 = SystemsOfUnits.Converter.ConvertToSI(specs("C").SpecUnit, specs("C").SpecValue)
            spval2 = SystemsOfUnits.Converter.ConvertToSI(specs("R").SpecUnit, specs("R").SpecValue)

            Select Case specs("C").SType
                Case ColumnSpec.SpecType.Component_Fraction, _
                ColumnSpec.SpecType.Component_Mass_Flow_Rate, _
                ColumnSpec.SpecType.Component_Molar_Flow_Rate, _
                ColumnSpec.SpecType.Component_Recovery, _
                ColumnSpec.SpecType.Temperature
                    Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCUnsupportedError1"))
            End Select
            Select Case specs("R").SType
                Case ColumnSpec.SpecType.Component_Fraction, _
                ColumnSpec.SpecType.Component_Mass_Flow_Rate, _
                ColumnSpec.SpecType.Component_Molar_Flow_Rate, _
                ColumnSpec.SpecType.Component_Recovery, _
                ColumnSpec.SpecType.Temperature
                    Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCUnsupportedError1"))
            End Select

            'step1

            Dim rr, B, D2 As Double

            'step2

            Dim i, j As Integer

            For i = 0 To ns
                Array.Resize(fcj(i), nc)
                Array.Resize(xc(i), nc)
                Array.Resize(yc(i), nc)
                Array.Resize(lc(i), nc)
                Array.Resize(vc(i), nc)
                Array.Resize(zc(i), nc)
                Array.Resize(zc(i), nc)
                Array.Resize(K(i), nc)
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

                Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, ns + 1, poptions,
                                                         Sub(ipar)
                                                             If IdealH Then
                                                                 Hl(ipar) = ppr.DW_CalcEnthalpy(x(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * ppr.AUX_MMM(x(ipar)) / 1000
                                                                 Hv(ipar) = ppr.DW_CalcEnthalpy(y(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * ppr.AUX_MMM(y(ipar)) / 1000
                                                             Else
                                                                 Hl(ipar) = pp.DW_CalcEnthalpy(x(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(x(ipar)) / 1000
                                                                 Hv(ipar) = pp.DW_CalcEnthalpy(y(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * pp.AUX_MMM(y(ipar)) / 1000
                                                             End If
                                                         End Sub),
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                      Settings.AppTaskScheduler)
                task1.Wait()

            Else
                For i = 0 To ns
                    If IdealH Then
                        ppr.CurrentMaterialStream.Flowsheet.CheckStatus()
                        Hl(i) = ppr.DW_CalcEnthalpy(x(i), Tj(i), P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(x(i)) / 1000
                        Hv(i) = ppr.DW_CalcEnthalpy(y(i), Tj(i), P(i), PropertyPackages.State.Vapor) * ppr.AUX_MMM(y(i)) / 1000
                    Else
                        pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                        Hl(i) = pp.DW_CalcEnthalpy(x(i), Tj(i), P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(x(i)) / 1000
                        Hv(i) = pp.DW_CalcEnthalpy(y(i), Tj(i), P(i), PropertyPackages.State.Vapor) * pp.AUX_MMM(y(i)) / 1000
                    End If
                Next
            End If

            Select Case specs("C").SType
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    LSSj(0) = spval1 / pp.AUX_MMM(x(0)) * 1000
                    rr = (Lj(0) + LSSj(0)) / LSSj(0)
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                    LSSj(0) = spval1
                    rr = (Lj(0) + LSSj(0)) / LSSj(0)
                Case ColumnSpec.SpecType.Stream_Ratio
                    rr = spval1
                Case ColumnSpec.SpecType.Heat_Duty
                    Q(0) = spval1
                    LSSj(0) = -Lj(0) - (Q(0) - Vj(1) * Hv(1) - F(0) * Hfj(0) + (Vj(0) + VSSj(0)) * Hv(0)) / Hl(0)
                    rr = (Lj(0) + LSSj(0)) / LSSj(0)
            End Select

            Select Case specs("R").SType
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    B = spval2 / pp.AUX_MMM(x(ns)) * 1000
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                    B = spval2
                Case ColumnSpec.SpecType.Stream_Ratio
                    B = Vj(ns) / spval2
                Case ColumnSpec.SpecType.Heat_Duty
                    Q(ns) = spval2
                    Dim sum3, sum4, val1 As Double
                    sum3 = 0
                    sum4 = 0
                    For i = 0 To ns
                        sum3 += F(i) * Hfj(i) - LSSj(i) * Hl(i) - VSSj(i) * Hv(i)
                    Next
                    val1 = sum3 - Q(ns)
                    sum4 = 0
                    For i = 0 To ns - 1
                        sum4 += Q(i) '- Lj(ns) * Hl(ns)
                    Next
                    B = -(val1 - (sum4 - Vj(0) * Hv(0))) / Hl(ns)
            End Select

            If condt = Column.condtype.Full_Reflux Then
                Vj(0) = sumF - B - sumLSS - sumVSS
                LSSj(0) = 0.0#
            Else
                D2 = sumF - B - sumLSS - sumVSS - Vj(0)
                LSSj(0) = D2
            End If


            'step3

            'internal loop

            ic = 0
            Do

                Dim il_err As Double = 0.0#
                Dim il_err_ant As Double = 0.0#
                Dim num, denom, x0, fx0 As New ArrayList

                'step4

                'find component liquid flows by the tridiagonal matrix method

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

                'tomich
                If doparallel Then

                    Dim t1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, nc, poptions,
                                                                 Sub(ipar)
                                                                         xt(ipar) = Tomich.TDMASolve(at(ipar), bt(ipar), ct(ipar), dt(ipar))
                                                                     End Sub),
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                      Settings.AppTaskScheduler)
                    t1.Wait()

                Else
                    For i = 0 To nc - 1
                        xt(i) = Tomich.TDMASolve(at(i), bt(i), ct(i), dt(i))
                    Next
                End If

                Dim sumx(ns), sumy(ns) As Double

                For i = 0 To ns
                    sumx(i) = 0
                    For j = 0 To nc - 1
                        lc(i)(j) = xt(j)(i)
                        If lc(i)(j) < 0.0# Then lc(i)(j) = 0.0000001
                        sumx(i) += lc(i)(j)
                    Next
                Next

                For i = 0 To ns
                    For j = 0 To nc - 1
                        If sumx(i) > 0.0# Then xc(i)(j) = lc(i)(j) / sumx(i) Else xc(i)(j) = yc(i)(j) / K(i)(j)
                    Next
                Next

                For i = 0 To ns
                    Lj(i) = 0
                    For j = 0 To nc - 1
                        lc(i)(j) = xt(j)(i)
                        Lj(i) += lc(i)(j)
                    Next
                    If Lj(i) < 0.0# Then Lj(i) = 0.001
                Next

                Dim tmp As Object

                'calculate new temperatures

                For i = 0 To ns
                    Tj_ant(i) = Tj(i)
                Next

                If doparallel Then
                    Dim t1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, ns + 1, poptions,
                                                                 Sub(ipar)
                                                                     If IdealK Then
                                                                         Dim tmpvar As Object = ppr.DW_CalcBubT(xc(ipar), P(ipar), Tj(ipar), K(ipar), True)
                                                                         Tj(ipar) = tmpvar(4)
                                                                         K(ipar) = tmpvar(6)
                                                                     Else
                                                                         Dim tmpvar As Object = pp.DW_CalcBubT(xc(ipar), P(ipar), Tj(ipar), K(ipar), True)
                                                                         Tj(ipar) = tmpvar(4)
                                                                         K(ipar) = tmpvar(6)
                                                                     End If
                                                                     If Tj(ipar) < 0 Or Double.IsNaN(Tj(ipar)) Then Tj(ipar) = Tj_ant(ipar)
                                                                 End Sub),
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                      Settings.AppTaskScheduler)
                    t1.Wait()
                Else
                    For i = 0 To ns
                        If IdealK Then
                            ppr.CurrentMaterialStream.Flowsheet.CheckStatus()
                            tmp = ppr.DW_CalcBubT(xc(i), P(i), Tj(i), K(i), True)
                        Else
                            pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                            tmp = pp.DW_CalcBubT(xc(i), P(i), Tj(i), K(i), True)
                        End If
                        Tj(i) = tmp(4)
                        K(i) = tmp(6)
                        If Tj(i) < 0 Or Double.IsNaN(Tj(i)) Then Tj(i) = Tj_ant(i)
                    Next
                End If

                For i = 0 To ns
                    dTj(i) = Tj(i) - Tj_ant(i)
                    If Abs(dTj(i)) > 10 Then Tj(i) = Math.Sign(dTj(i)) * 10 + Tj_ant(i)
                    If Double.IsNaN(Tj(i)) Or Double.IsInfinity(Tj(i)) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))
                Next

                For i = 0 To ns
                    For j = 0 To nc - 1
                        If Double.IsNaN(K(i)(j)) Then K(i)(j) = pp.AUX_PVAPi(j, Tj(i)) / P(i)
                    Next
                    If Double.IsNaN(Tj(i)) Or Double.IsInfinity(Tj(i)) Then Tj(i) = Tj_ant(i)
                Next

                t_error_ant = t_error
                t_error = 0.0#
                For i = 0 To ns
                    t_error += (Tj(i) - Tj_ant(i)) ^ 2
                Next

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

                If doparallel Then

                    Dim t1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, ns + 1, poptions,
                                                                                     Sub(ipar)
                                                                                         If IdealH Then
                                                                                             Hl(ipar) = ppr.DW_CalcEnthalpy(xc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(ipar)) / 1000
                                                                                             Hv(ipar) = ppr.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(ipar)) / 1000
                                                                                         Else
                                                                                             Hl(ipar) = pp.DW_CalcEnthalpy(xc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(ipar)) / 1000
                                                                                             Hv(ipar) = pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(ipar)) / 1000
                                                                                         End If
                                                                                     End Sub),
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                      Settings.AppTaskScheduler)
                    t1.Wait()

                Else
                    For i = 0 To ns
                        If IdealH Then
                            ppr.CurrentMaterialStream.Flowsheet.CheckStatus()
                            Hl(i) = ppr.DW_CalcEnthalpy(xc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(i)) / 1000
                            Hv(i) = ppr.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(i)) / 1000
                        Else
                            pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                            Hl(i) = pp.DW_CalcEnthalpy(xc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(i)) / 1000
                            Hv(i) = pp.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(i)) / 1000
                        End If
                    Next
                End If

                'handle specs

                Select Case specs("C").SType
                    Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                        LSSj(0) = spval1 / pp.AUX_MMM(xc(0)) * 1000
                        rr = (Lj(0) + LSSj(0)) / LSSj(0)
                    Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                        LSSj(0) = spval1
                        rr = (Lj(0) + LSSj(0)) / LSSj(0)
                    Case ColumnSpec.SpecType.Stream_Ratio
                        rr = spval1
                    Case ColumnSpec.SpecType.Heat_Duty
                        Q(0) = spval1
                        LSSj(0) = -Lj(0) - (Q(0) - Vj(1) * Hv(1) - F(0) * Hfj(0) + (Vj(0) + VSSj(0)) * Hv(0)) / Hl(0)
                        rr = (Lj(0) + LSSj(0)) / LSSj(0)
                End Select

                Select Case specs("R").SType
                    Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                        B = spval2 / pp.AUX_MMM(xc(ns)) * 1000
                    Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                        B = spval2
                    Case ColumnSpec.SpecType.Stream_Ratio
                        B = Vj(ns) / spval2
                    Case ColumnSpec.SpecType.Heat_Duty
                        Q(ns) = spval2
                        Dim sum3, sum4, val1 As Double
                        sum3 = 0
                        sum4 = 0
                        For i = 0 To ns
                            sum3 += F(i) * Hfj(i) - LSSj(i) * Hl(i) - VSSj(i) * Hv(i)
                        Next
                        val1 = sum3 - Q(ns)
                        sum4 = 0
                        For i = 0 To ns - 1
                            sum4 += Q(i)
                        Next
                        B = -(val1 - (sum4 - Vj(0) * Hv(0))) / Hl(ns)
                End Select

                sumF = 0
                sumLSS = 0
                sumVSS = 0
                For i = 0 To ns
                    sumF += F(i)
                    If i > 0 Then sumLSS += LSS(i)
                    sumVSS += VSS(i)
                Next

                If condt = Column.condtype.Full_Reflux Then
                    Vj(0) = sumF - B - sumLSS - sumVSS
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

                If Not condt = Column.condtype.Full_Reflux Then Vj(0) = V(0)
                Vj(1) = (rr + 1) * LSSj(0) - Fj(0) + Vj(0)
                For i = 2 To ns
                    Vj(i) = (gamma(i - 1) - alpha(i - 1) * Vj(i - 1)) / beta(i - 1)
                    If Vj(i) < 0 Then Vj(i) = 0.000001
                Next

                For i = 0 To ns
                    dVj(i) = Vj(i) - Vj_ant(i)
                    If Abs(dVj(i)) > 0.1 * Vj_ant(i) Then Vj(i) = Vj_ant(i) * (1 + Math.Sign(dVj(i)) * 0.1)
                Next

                'Ljs
                For i = 0 To ns
                    If i < ns Then Lj(i) = Vj(i + 1) + sum1(i) - Vj(0) Else Lj(i) = sum1(i) - Vj(0)
                    If Lj(i) < 0.0# Then Lj(i) = 0.0001 * Fj.Sum
                Next

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

                ic = ic + 1

                If Not IdealH And Not IdealK Then
                    If ic >= maxits Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCMaxIterationsReached"))
                End If
                If Double.IsNaN(t_error) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))
                If ic = stopatitnumber - 1 Then Exit Do

                pp.CurrentMaterialStream.Flowsheet.CheckStatus()

                Calculator.WriteToConsole("Bubble Point solver T error = " & t_error, 1)

            Loop Until t_error < tol(1)

            'finished, return arrays

            Return New Object() {Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ic, t_error}

        End Function

    End Class

    <System.Serializable()> Public Class BurninghamOttoMethod

        Public Shared Function Solve(ByVal nc As Integer, ByVal ns As Integer, ByVal maxits As Integer, _
                                ByVal tol As Array, ByVal F As Array, ByVal V As Array, _
                                ByVal Q As Array, ByVal L As Array, _
                                ByVal VSS As Array, ByVal LSS As Array, ByVal Kval()() As Double, _
                                ByVal x()() As Double, ByVal y()() As Double, ByVal z()() As Double, _
                                ByVal fc()() As Double, _
                                ByVal HF As Array, ByVal T As Array, ByVal P As Array, _
                                ByVal stopatitnumber As Integer, _
                                ByVal eff() As Double, _
                                ByVal pp As PropertyPackages.PropertyPackage, _
                                ByVal specs As Dictionary(Of String, SepOps.ColumnSpec), _
                                ByVal IdealK As Boolean, ByVal IdealH As Boolean,
                                Optional ByVal llextr As Boolean = False) As Object

            Dim ppr As PropertyPackages.RaoultPropertyPackage = Nothing

            If IdealK Or IdealH Then
                ppr = New PropertyPackages.RaoultPropertyPackage
                ppr.CurrentMaterialStream = pp.CurrentMaterialStream
            End If

            Dim doparallel As Boolean = Settings.EnableParallelProcessing
            Dim poptions As New ParallelOptions() With {.MaxDegreeOfParallelism = Settings.MaxDegreeOfParallelism}

            Dim ic As Integer
            Dim t_error, comperror As Double
            Dim Tj(ns), Tj_ant(ns) As Double
            Dim Fj(ns), Lj(ns), Vj(ns), Vj_ant(ns), xc(ns)(), fcj(ns)(), yc(ns)(), yc_ant(ns)(), lc(ns)(), vc(ns)(), zc(ns)(), K(ns)() As Double
            Dim Hfj(ns), Hv(ns), Hl(ns) As Double
            Dim VSSj(ns), LSSj(ns) As Double
            Dim sum1(ns), sum2(ns), sum3(ns) As Double

            'step1

            'step2

            Dim i, j As Integer

            For i = 0 To ns
                Array.Resize(fcj(i), nc)
                Array.Resize(xc(i), nc)
                Array.Resize(yc(i), nc)
                Array.Resize(yc_ant(i), nc)
                Array.Resize(lc(i), nc)
                Array.Resize(vc(i), nc)
                Array.Resize(zc(i), nc)
                Array.Resize(zc(i), nc)
                Array.Resize(K(i), nc)
            Next

            For i = 0 To ns
                VSSj(i) = VSS(i)
                LSSj(i) = LSS(i)
                Lj(i) = L(i)
                Vj(i) = V(i)
                Tj(i) = T(i)
                Fj(i) = F(i)
                Hfj(i) = HF(i) / 1000
                fcj(i) = fc(i)
            Next

            For i = 0 To ns
                For j = 0 To nc - 1
                    K(i)(j) = Kval(i)(j)
                    If Double.IsNaN(K(i)(j)) Or Double.IsInfinity(K(i)(j)) Or K(i)(j) = 0# Then K(i)(j) = pp.AUX_PVAPi(j, T(i)) / P(i)
                Next
            Next

            'step3

            'internal loop
            ic = 0
            Do

                Dim il_err As Double = 0.0#
                Dim il_err_ant As Double = 0.0#
                Dim num, denom, x0, fx0 As New ArrayList

                'step4

                'find component liquid flows by the tridiagonal matrix method

                Dim at(nc - 1)(), bt(nc - 1)(), ct(nc - 1)(), dt(nc - 1)(), xt(nc - 1)() As Double

                For i = 0 To nc - 1
                    Array.Resize(at(i), ns + 1)
                    Array.Resize(bt(i), ns + 1)
                    Array.Resize(ct(i), ns + 1)
                    Array.Resize(dt(i), ns + 1)
                    Array.Resize(xt(i), ns + 1)
                Next

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

                'tomich
                For i = 0 To nc - 1
                    xt(i) = Tomich.TDMASolve(at(i), bt(i), ct(i), dt(i))
                Next

                Dim sumx(ns), sumy(ns), sumz(ns) As Double

                For i = 0 To ns
                    sumx(i) = 0
                    For j = 0 To nc - 1
                        lc(i)(j) = xt(j)(i)
                        If lc(i)(j) < 0 Then lc(i)(j) = 0.0000001
                        sumx(i) += lc(i)(j)
                    Next
                Next

                'Ljs
                For i = 0 To ns
                    'If sumx(i) > 1.5 Then
                    '    Lj(i) = Lj(i) * 1.5
                    'ElseIf sumx(i) < 0.5 Then
                    '    Lj(i) = Lj(i) * 0.5
                    'Else
                    Lj(i) = Lj(i) * sumx(i)
                    'End If
                Next

                For i = 0 To ns
                    For j = 0 To nc - 1
                        xc(i)(j) = lc(i)(j) / sumx(i)
                        yc(i)(j) = xc(i)(j) * K(i)(j)
                        sumy(i) += yc(i)(j)
                    Next
                Next

                For i = 0 To ns
                    For j = 0 To nc - 1
                        yc(i)(j) = yc(i)(j) / sumy(i)
                    Next
                Next

                For i = 0 To ns
                    sum3(i) = 0
                    For j = i To ns
                        sum3(i) += Fj(j) - LSSj(j) - VSSj(j)
                    Next
                Next

                'solve matrices

                For i = 0 To ns
                    Vj_ant(i) = Vj(i)
                Next

                For i = 0 To ns
                    If i > 0 Then
                        Vj(i) = Lj(i - 1) - Lj(ns) + sum3(i)
                    Else
                        Vj(i) = -Lj(ns) + sum3(i)
                    End If
                    If Vj(i) < 0 Then Vj(i) = 0.01 '-Vj(i)
                Next

                For i = 0 To ns
                    sumz(i) = 0
                    For j = 0 To nc - 1
                        vc(i)(j) = xc(i)(j) * Vj(i) * K(i)(j)
                        zc(i)(j) = (lc(i)(j) + vc(i)(j)) / (Lj(i) + Vj(i))
                        sumz(i) += zc(i)(j)
                    Next
                Next

                For i = 0 To ns
                    For j = 0 To nc - 1
                        zc(i)(j) = zc(i)(j) / sumz(i)
                    Next
                Next

                'Dim tmp As Object

                'calculate new temperatures

                ''''''''''''''''''''
                Dim H(ns), dHldT(ns), dHvdT(ns), dHdTa(ns), dHdTb(ns), dHdTc(ns), dHl(ns), dHv(ns) As Double

                If doparallel Then

                    Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, ns + 1, poptions,
                                                             Sub(ipar)
                                                                 If IdealH Then
                                                                     Hl(ipar) = ppr.DW_CalcEnthalpy(xc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(ipar)) / 1000
                                                                     dHl(ipar) = ppr.DW_CalcEnthalpy(xc(ipar), Tj(ipar) - 1, P(ipar), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(ipar)) / 1000
                                                                     If llextr Then
                                                                         Hv(ipar) = ppr.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * ppr.AUX_MMM(yc(ipar)) / 1000
                                                                         dHv(ipar) = ppr.DW_CalcEnthalpy(yc(ipar), Tj(ipar) - 1, P(ipar), PropertyPackages.State.Liquid) * ppr.AUX_MMM(yc(ipar)) / 1000
                                                                     Else
                                                                         Hv(ipar) = ppr.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(ipar)) / 1000
                                                                         dHv(ipar) = ppr.DW_CalcEnthalpy(yc(ipar), Tj(ipar) - 1, P(ipar), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(ipar)) / 1000
                                                                     End If
                                                                 Else
                                                                     Hl(ipar) = pp.DW_CalcEnthalpy(xc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(ipar)) / 1000
                                                                     dHl(ipar) = pp.DW_CalcEnthalpy(xc(ipar), Tj(ipar) - 1, P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(ipar)) / 1000
                                                                     If llextr Then
                                                                         Hv(ipar) = pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(yc(ipar)) / 1000
                                                                         dHv(ipar) = pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar) - 1, P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(yc(ipar)) / 1000
                                                                     Else
                                                                         Hv(ipar) = pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(ipar)) / 1000
                                                                         dHv(ipar) = pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar) - 1, P(ipar), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(ipar)) / 1000
                                                                     End If
                                                                 End If
                                                             End Sub),
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                      Settings.AppTaskScheduler)

                    task1.Wait(30000)
                Else
                    If IdealH Then
                        For i = 0 To ns
                            Hl(i) = ppr.DW_CalcEnthalpy(xc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(i)) / 1000
                            dHl(i) = ppr.DW_CalcEnthalpy(xc(i), Tj(i) - 1, P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(i)) / 1000
                            If llextr Then
                                Hv(i) = ppr.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(yc(i)) / 1000
                                dHv(i) = ppr.DW_CalcEnthalpy(yc(i), Tj(i) - 1, P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(yc(i)) / 1000
                            Else
                                Hv(i) = ppr.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(i)) / 1000
                                dHv(i) = ppr.DW_CalcEnthalpy(yc(i), Tj(i) - 1, P(i), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(i)) / 1000
                            End If
                            ppr.CurrentMaterialStream.Flowsheet.CheckStatus()
                        Next
                    Else
                        For i = 0 To ns
                            Hl(i) = pp.DW_CalcEnthalpy(xc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(i)) / 1000
                            dHl(i) = pp.DW_CalcEnthalpy(xc(i), Tj(i) - 1, P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(i)) / 1000
                            If llextr Then
                                Hv(i) = pp.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(yc(i)) / 1000
                                dHv(i) = pp.DW_CalcEnthalpy(yc(i), Tj(i) - 1, P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(yc(i)) / 1000
                            Else
                                Hv(i) = pp.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(i)) / 1000
                                dHv(i) = pp.DW_CalcEnthalpy(yc(i), Tj(i) - 1, P(i), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(i)) / 1000
                            End If
                            pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                        Next
                    End If
                End If

                For i = 0 To ns
                    If i = 0 Then
                        H(i) = Vj(i + 1) * Hv(i + 1) + Fj(i) * Hfj(i) - (Lj(i) + LSSj(i)) * Hl(i) - (Vj(i) + VSSj(i)) * Hv(i) - Q(i)
                    ElseIf i = ns Then
                        H(i) = Lj(i - 1) * Hl(i - 1) + Fj(i) * Hfj(i) - (Lj(i) + LSSj(i)) * Hl(i) - (Vj(i) + VSSj(i)) * Hv(i) - Q(i)
                    Else
                        H(i) = Lj(i - 1) * Hl(i - 1) + Vj(i + 1) * Hv(i + 1) + Fj(i) * Hfj(i) - (Lj(i) + LSSj(i)) * Hl(i) - (Vj(i) + VSSj(i)) * Hv(i) - Q(i)
                    End If
                    dHldT(i) = (Hl(i) - dHl(i)) / 1
                    dHvdT(i) = (Hv(i) - dHv(i)) / 1
                Next

                For i = 0 To ns
                    If i > 0 Then dHdTa(i) = Lj(i - 1) * dHldT(i - 1)
                    dHdTb(i) = -(Lj(i) + LSSj(i)) * dHldT(i) - (Vj(i) + VSSj(i)) * dHvdT(i)
                    If i < ns Then dHdTc(i) = Vj(i + 1) * dHvdT(i + 1)
                Next

                Dim ath(ns), bth(ns), cth(ns), dth(ns), xth(ns) As Double

                For i = 0 To ns
                    dth(i) = -H(i)
                    bth(i) = dHdTb(i)
                    If i < ns Then cth(i) = dHdTc(i)
                    If i > 0 Then ath(i) = dHdTa(i)
                Next

                'solve matrices
                'tomich

                xth = Tomich.TDMASolve(ath, bth, cth, dth)

                Dim tmp As Object

                t_error = 0.0#
                comperror = 0.0#
                For i = 0 To ns
                    Tj_ant(i) = Tj(i)
                    If Abs(xth(i)) > 0.1 * Tj(i) Then
                        Tj(i) = Tj(i) + Sign(xth(i)) * 0.1 * Tj(i)
                    Else
                        Tj(i) = Tj(i) + xth(i)
                    End If
                    If Double.IsNaN(Tj(i)) Or Double.IsInfinity(Tj(i)) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))
                    If IdealK Then
                        If llextr Then
                            tmp = ppr.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i), "LL")
                        Else
                            tmp = ppr.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i))
                        End If
                    Else
                        If llextr Then
                            tmp = pp.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i), "LL")
                        Else
                            tmp = pp.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i))
                        End If
                    End If
                    sumy(i) = 0
                    For j = 0 To nc - 1
                        K(i)(j) = tmp(j)
                        If Double.IsNaN(K(i)(j)) Or Double.IsInfinity(K(i)(j)) Then
                            If llextr Then
                                K(i)(j) = 1.0#
                            Else
                                K(i)(j) = pp.AUX_PVAPi(j, Tj(i)) / P(i)
                            End If
                        End If
                        yc_ant(i)(j) = yc(i)(j)
                        yc(i)(j) = K(i)(j) * xc(i)(j)
                        sumy(i) += yc(i)(j)
                        comperror += Abs(yc(i)(j) - yc_ant(i)(j)) ^ 2
                    Next
                    t_error += Abs(Tj(i) - Tj_ant(i)) ^ 2
                    pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                Next

                For i = 0 To ns
                    For j = 0 To nc - 1
                        yc(i)(j) = yc(i)(j) / sumy(i)
                    Next
                Next

                ic = ic + 1

                If Not IdealH And Not IdealK Then
                    If ic >= maxits Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCMaxIterationsReached"))
                End If

                If Double.IsNaN(t_error) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))
                If Double.IsNaN(comperror) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))

                pp.CurrentMaterialStream.Flowsheet.CheckStatus()

                Calculator.WriteToConsole("Sum Rates solver T error = " & t_error, 1)
                Calculator.WriteToConsole("Sum Rates solver composition error = " & comperror, 1)

            Loop Until t_error <= tol(1) And comperror <= tol(1)

            ' finished, return arrays

            Return New Object() {Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ic, t_error}

        End Function

    End Class

    <System.Serializable()> Public Class NaphtaliSandholmMethod

        Sub New()

        End Sub

        Dim ndeps As Double = 0.000001

        Dim _nc, _ns As Integer
        Dim _VSS, _LSS As Double()
        Dim _spval1, _spval2 As Double
        Dim _spci1, _spci2 As Integer
        Dim _eff, _F, _Q, _P, _HF As Double()
        Dim _fc()(), _maxF As Double
        Public _pp As PropertyPackages.PropertyPackage
        Public _ppr As PropertyPackages.RaoultPropertyPackage
        Dim _coltype As Column.ColType
        Dim _specs As Dictionary(Of String, SepOps.ColumnSpec)
        Dim _bx, _dbx As Double()
        Dim _condtype As DistillationColumn.condtype
        Dim llextr As Boolean = False
        Dim _Kval()() As Double
        Dim _maxT, _maxvc, _maxlc As Double

        Private grad As Boolean = False

        Private ik, ih As Boolean

        Private Function GetSolver(solver As OptimizationMethod) As SwarmOps.Optimizer

            Select Case solver
                Case OptimizationMethod.DifferentialEvolution
                    Return New SwarmOps.Optimizers.DE()
                Case OptimizationMethod.GradientDescent
                    Return New SwarmOps.Optimizers.GD()
                Case OptimizationMethod.LocalUnimodalSampling
                    Return New SwarmOps.Optimizers.LUS()
                Case OptimizationMethod.ManyOptimizingLiaisons
                    Return New SwarmOps.Optimizers.MOL()
                Case OptimizationMethod.Mesh
                    Return New SwarmOps.Optimizers.MESH()
                Case OptimizationMethod.ParticleSwarm
                    Return New SwarmOps.Optimizers.PS()
                Case OptimizationMethod.ParticleSwarmOptimization
                    Return New SwarmOps.Optimizers.PSO()
                Case Else
                    Return Nothing
            End Select

        End Function

        Public Function FunctionValue(ByVal xl() As Double) As Double

            Dim x As Double() = xl.ExpY

            If x.Where(Function(xi) xi < 0#).Count > 0 Then
                Return x.AbsSqrSumY * 100
            End If

            Dim doparallel As Boolean = Settings.EnableParallelProcessing
            Dim poptions As New ParallelOptions() With {.MaxDegreeOfParallelism = Settings.MaxDegreeOfParallelism, .TaskScheduler = Settings.AppTaskScheduler}

            Dim nc, ns As Integer
            Dim i, j As Integer
            Dim VSS, LSS, F, Q, P, HF, eff As Double()
            Dim fc()() As Double
            Dim spval1, spval2, spfval1, spfval2, maxF As Double
            Dim spci1, spci2 As Integer
            Dim coltype As Column.ColType = _coltype

            F = _F
            Q = _Q
            P = _P
            HF = _HF
            eff = _eff
            fc = _fc
            maxF = _maxF

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
            Dim Tj(ns), vc(ns)(), lc(ns)(), zc(ns)(), Vj(ns), Lj(ns), xc(ns)(), yc(ns)(), Kval(ns)() As Double

            For i = 0 To ns
                Array.Resize(lc(i), nc)
                Array.Resize(vc(i), nc)
                Array.Resize(zc(i), nc)
                Array.Resize(xc(i), nc)
                Array.Resize(yc(i), nc)
                Array.Resize(Kval(i), nc)
            Next


            For i = 0 To ns
                Tj(i) = x(i * (2 * nc + 1)) * _maxT
                If Double.IsNaN(Tj(i)) Or Double.IsInfinity(Tj(i)) Then Throw New Exception(_pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))
                For j = 0 To nc - 1
                    vc(i)(j) = x(i * (2 * nc + 1) + j + 1) * _maxvc
                    lc(i)(j) = x(i * (2 * nc + 1) + j + 1 + nc) * _maxlc
                Next
            Next

            Dim VSSj(ns), LSSj(ns), Hv(ns), Hl(ns), Hv0(ns), Hl0(ns) As Double
            Dim sumvkj(ns), sumlkj(ns) As Double

            Dim M(ns, nc - 1), E(ns, nc - 1), H(ns) As Double
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
                If i > 0 Then Vj(i) = sumvkj(i)
                Lj(i) = sumlkj(i)
            Next

            For i = 0 To ns
                For j = 0 To nc - 1
                    yc(i)(j) = vc(i)(j) / sumvkj(i)
                Next
                For j = 0 To nc - 1
                    If sumlkj(i) > 0.0# Then xc(i)(j) = lc(i)(j) / sumlkj(i) Else xc(i)(j) = yc(i)(j) / (_pp.AUX_PVAPi(j, Tj(i)) / P(i))
                Next
            Next

            For i = 0 To ns
                For j = 0 To nc - 1
                    zc(i)(j) = (lc(i)(j) + vc(i)(j))
                Next
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
            If _condtype = Column.condtype.Full_Reflux Then
                Vj(0) = 1 - Lj(ns) - sumLSS - sumVSS
                LSSj(0) = 0.0#
            Else
                LSSj(0) = 1 - Lj(ns) - sumLSS - sumVSS - Vj(0)
            End If

            For i = 0 To ns
                If Vj(i) <> 0 Then Sv(i) = VSSj(i) / Vj(i) Else Sv(i) = 0
                If Lj(i) <> 0 Then Sl(i) = LSSj(i) / Lj(i) Else Sl(i) = 0
            Next

            'calculate K-values

            If doparallel Then

                Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, ns + 1, poptions,
                                                         Sub(ipar)
                                                             Dim tmp0 As Object
                                                             If ik Then
                                                                 If llextr Then
                                                                     tmp0 = _ppr.DW_CalcKvalue(xc(ipar), yc(ipar), Tj(ipar), P(ipar), "LL")
                                                                 Else
                                                                     tmp0 = _ppr.DW_CalcKvalue(xc(ipar), yc(ipar), Tj(ipar), P(ipar))
                                                                 End If
                                                             Else
                                                                 If llextr Then
                                                                     tmp0 = _pp.DW_CalcKvalue(xc(ipar), yc(ipar), Tj(ipar), P(ipar), "LL")
                                                                 Else
                                                                     tmp0 = _pp.DW_CalcKvalue(xc(ipar), yc(ipar), Tj(ipar), P(ipar))
                                                                 End If
                                                             End If
                                                             Dim jj As Integer
                                                             For jj = 0 To nc - 1
                                                                 Kval(ipar)(jj) = tmp0(jj)
                                                             Next
                                                         End Sub),
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                      Settings.AppTaskScheduler)
                task1.Wait()

            Else
                Dim tmp0 As Object
                For i = 0 To ns
                    If ik Then
                        If llextr Then
                            tmp0 = _ppr.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i), "LL")
                        Else
                            tmp0 = _ppr.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i))
                        End If
                    Else
                        If llextr Then
                            tmp0 = _pp.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i), "LL")
                        Else
                            tmp0 = _pp.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i))
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

                Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, ns + 1, poptions,
                                                         Sub(ipar)
                                                             If Vj(ipar) <> 0.0# Then
                                                                 If ih Then
                                                                     If llextr Then
                                                                         Hv(ipar) = _ppr.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * _ppr.AUX_MMM(yc(ipar)) / 1000
                                                                     Else
                                                                         Hv(ipar) = _ppr.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * _ppr.AUX_MMM(yc(ipar)) / 1000
                                                                     End If
                                                                 Else
                                                                     If llextr Then
                                                                         Hv(ipar) = _pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * _pp.AUX_MMM(yc(ipar)) / 1000
                                                                     Else
                                                                         Hv(ipar) = _pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * _pp.AUX_MMM(yc(ipar)) / 1000
                                                                     End If
                                                                 End If
                                                             Else
                                                                 Hv(ipar) = 0.0#
                                                             End If
                                                             If Lj(ipar) <> 0 Then
                                                                 If ih Then
                                                                     Hl(ipar) = _ppr.DW_CalcEnthalpy(xc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * _ppr.AUX_MMM(xc(ipar)) / 1000
                                                                 Else
                                                                     Hl(ipar) = _pp.DW_CalcEnthalpy(xc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * _pp.AUX_MMM(xc(ipar)) / 1000
                                                                 End If
                                                             Else
                                                                 Hl(ipar) = 0.0#
                                                             End If
                                                         End Sub),
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                      Settings.AppTaskScheduler)
                task1.Wait()

            Else
                For i = 0 To ns
                    If Vj(i) <> 0 Then
                        If ih Then
                            If llextr Then
                                Hv(i) = _ppr.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * _ppr.AUX_MMM(yc(i)) / 1000
                            Else
                                Hv(i) = _ppr.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Vapor) * _ppr.AUX_MMM(yc(i)) / 1000
                            End If
                        Else
                            If llextr Then
                                Hv(i) = _pp.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * _pp.AUX_MMM(yc(i)) / 1000
                            Else
                                Hv(i) = _pp.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Vapor) * _pp.AUX_MMM(yc(i)) / 1000
                            End If
                        End If
                    Else
                        Hv(i) = 0
                    End If
                    If Lj(i) <> 0 Then
                        If ih Then
                            Hl(i) = _ppr.DW_CalcEnthalpy(xc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * _ppr.AUX_MMM(xc(i)) / 1000
                        Else
                            Hl(i) = _pp.DW_CalcEnthalpy(xc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * _pp.AUX_MMM(xc(i)) / 1000
                        End If
                    Else
                        Hl(i) = 0
                    End If
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
                    If _specs("C").SpecUnit = "M" Then
                        spfval1 = xc(0)(spci1) - spval1
                    Else 'W
                        spfval1 = _pp.AUX_CONVERT_MOL_TO_MASS(xc(0))(spci1) - spval1
                    End If
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    spfval1 = LSSj(0) * xc(0)(spci1) - spval1 / _pp.RET_VMM()(spci1) * 1000 / maxF
                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                    spfval1 = LSSj(0) * xc(0)(spci1) - spval1 / maxF
                Case ColumnSpec.SpecType.Component_Recovery
                    Dim rec As Double = spval1 / 100
                    Dim sumc As Double = 0
                    For j = 0 To ns
                        sumc += fc(j)(spci1)
                    Next
                    sumc *= rec
                    If _specs("C").SpecUnit = "% M/M" Then
                        spfval1 = xc(0)(spci1) * LSSj(0) - sumc
                    Else '% W/W
                        spfval1 = _pp.RET_VMM()(spci1) * 1000 * (xc(0)(spci1) * LSSj(0) - sumc)
                    End If
                Case ColumnSpec.SpecType.Heat_Duty
                    Q(0) = spval1 / maxF
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    spfval1 = LSSj(0) - spval1 / _pp.AUX_MMM(xc(0)) * 1000 / maxF
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                    spfval1 = LSSj(0) - spval1 / maxF
                Case ColumnSpec.SpecType.Stream_Ratio
                    spfval1 = Lj(0) / LSSj(0) - spval1
                Case ColumnSpec.SpecType.Temperature
                    spfval1 = Tj(0) - spval1
            End Select

            Select Case _specs("R").SType
                Case ColumnSpec.SpecType.Component_Fraction
                    If _specs("R").SpecUnit = "M" Then
                        spfval2 = xc(ns)(spci2) - spval2
                    Else 'W
                        spfval2 = _pp.AUX_CONVERT_MOL_TO_MASS(xc(ns))(spci2) - spval2
                    End If
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    spfval2 = Lj(ns) * xc(ns)(spci2) - spval2 / _pp.RET_VMM()(spci2) * 1000 / maxF
                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                    spfval2 = Lj(ns) * xc(ns)(spci2) - spval2 / maxF
                Case ColumnSpec.SpecType.Component_Recovery
                    Dim rec As Double = spval2 / 100
                    Dim sumc As Double = 0
                    For j = 0 To ns
                        sumc += fc(j)(spci2)
                    Next
                    sumc *= rec
                    If _specs("R").SpecUnit = "% M/M" Then
                        spfval2 = lc(ns)(spci2) - sumc
                    Else '% W/W
                        spfval2 = _pp.RET_VMM()(spci2) * 1000 * (xc(0)(spci1) * Lj(ns) - sumc)
                    End If
                Case ColumnSpec.SpecType.Heat_Duty
                    Q(ns) = spval2 / maxF
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    spfval2 = Lj(ns) - spval2 / _pp.AUX_MMM(xc(ns)) * 1000 / maxF
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                    spfval2 = Lj(ns) - spval2 / maxF
                Case ColumnSpec.SpecType.Stream_Ratio
                    spfval2 = Vj(ns) / Lj(ns) - spval2
                Case ColumnSpec.SpecType.Temperature
                    spfval2 = Tj(ns) - spval2
            End Select

            For i = 0 To ns
                For j = 0 To nc - 1
                    M_ant(i, j) = M(i, j)
                    E_ant(i, j) = E(i, j)
                    H_ant(i) = H(i)
                    If i = 0 Then
                        M(i, j) = lc(i)(j) * (1 + Sl(i)) + vc(i)(j) * (1 + Sv(i)) - vc(i + 1)(j) - fc(i)(j)
                        If _condtype = Column.condtype.Full_Reflux Then
                            E(i, j) = -vc(i)(j) + (1 - eff(i)) * vc(i + 1)(j) * sumvkj(i) / sumvkj(i + 1)
                        Else
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
                    H(i) = (Hl(i) * (1 + Sl(i)) * sumlkj(i) + Hv(i) * (1 + Sv(i)) * sumvkj(i) - Hv(i + 1) * sumvkj(i + 1) - HF(i) * F(i) - Q(i))
                ElseIf i = ns Then
                    H(i) = (Hl(i) * (1 + Sl(i)) * sumlkj(i) + Hv(i) * (1 + Sv(i)) * sumvkj(i) - Hl(i - 1) * sumlkj(i - 1) - HF(i) * F(i) - Q(i))
                Else
                    H(i) = (Hl(i) * (1 + Sl(i)) * sumlkj(i) + Hv(i) * (1 + Sv(i)) * sumvkj(i) - Hl(i - 1) * sumlkj(i - 1) - Hv(i + 1) * sumvkj(i + 1) - HF(i) * F(i) - Q(i))
                End If
                H(i) /= 1000
                Select Case coltype
                    Case Column.ColType.DistillationColumn
                        If _condtype <> Column.condtype.Full_Reflux Then H(0) = spfval1 / spval1
                        H(ns) = spfval2 / spval2
                    Case Column.ColType.AbsorptionColumn
                        'do nothing
                    Case Column.ColType.ReboiledAbsorber
                        H(ns) = spfval2 / spval2
                    Case Column.ColType.RefluxedAbsorber
                        H(0) = spfval1 / spval1
                End Select
            Next

            If _condtype = Column.condtype.Total_Condenser Then
                Dim sum1 As Double = 0
                For j = 0 To nc - 1
                    sum1 += Kval(0)(j) * xc(0)(j)
                Next
                i = 0
                For j = 0 To nc - 1
                    If j = 0 Then
                        E(i, j) = 1 - sum1
                    Else
                        E(i, j) = xc(i)(j) - yc(i)(j)
                    End If
                Next
            End If

            Dim errors(x.Length - 1) As Double

            For i = 0 To ns
                errors(i * (2 * nc + 1)) = H(i)
                For j = 0 To nc - 1
                    errors(i * (2 * nc + 1) + j + 1) = M(i, j) / zc(i).SumY
                    errors(i * (2 * nc + 1) + j + 1 + nc) = E(i, j)
                Next
            Next

            If Not grad Then
                _pp.CurrentMaterialStream.Flowsheet.ShowMessage("NS solver: current objective function (error) value = " & errors.AbsSqrSumY, IFlowsheet.MessageType.Information)
                _pp.CurrentMaterialStream.Flowsheet.CheckStatus()
            End If

            For i = 0 To errors.Length - 1
                If Double.IsNaN(errors(i)) Or Double.IsInfinity(errors(i)) Then errors(i) = 10000000000.0
            Next

            Return errors.AbsSqrSumY

        End Function

        Public Function FunctionGradient(ByVal x() As Double) As Double()

            grad = True

            Dim epsilon As Double = ndeps

            Dim f1, f2 As Double
            Dim g(x.Length - 1), x1(x.Length - 1), x2(x.Length - 1) As Double
            Dim j, k As Integer

            For j = 0 To x.Length - 1
                For k = 0 To x.Length - 1
                    x1(k) = x(k)
                    x2(k) = x(k)
                Next
                If x(j) <> 0.0# Then
                    x1(j) = x(j) * (1.0# + epsilon)
                    x2(j) = x(j) * (1.0# - epsilon)
                Else
                    x1(j) = x(j) + epsilon
                    x2(j) = x(j) - epsilon
                End If
                f1 = FunctionValue(x1)
                f2 = FunctionValue(x2)
                g(j) = (f2 - f1) / (x2(j) - x1(j))
            Next

            _pp.CurrentMaterialStream.Flowsheet.CheckStatus()

            grad = False

            Return g

        End Function

        Public Function Solve(ByVal nc As Integer, ByVal ns As Integer, ByVal maxits As Integer,
                                ByVal tol As Array, ByVal F As Array, ByVal V As Array,
                                ByVal Q As Array, ByVal L As Array,
                                ByVal VSS As Array, ByVal LSS As Array, ByVal Kval()() As Double,
                                ByVal x()() As Double, ByVal y()() As Double, ByVal z()() As Double,
                                ByVal fc()() As Double,
                                ByVal HF As Array, ByVal T As Array, ByVal P As Array,
                                ByVal condt As DistillationColumn.condtype,
                                ByVal eff() As Double,
                                ByVal coltype As Column.ColType,
                                ByVal pp As PropertyPackages.PropertyPackage,
                                ByVal specs As Dictionary(Of String, SepOps.ColumnSpec),
                                ByVal epsilon As Double,
                                ByVal Solver As OptimizationMethod,
                                ByVal LowerBound As Double, ByVal UpperBound As Double,
                                ByVal SimplexPreconditioning As Boolean,
                                ByVal IdealK As Boolean, ByVal IdealH As Boolean,
                                Optional ByVal LLEX As Boolean = False) As Object

            ik = IdealK
            ih = IdealH

            If ik Or ih Then
                _ppr = New PropertyPackages.RaoultPropertyPackage
                _ppr.CurrentMaterialStream = pp.CurrentMaterialStream
            End If

            llextr = LLEX 'liquid-liquid extractor

            ndeps = epsilon

            Dim cv As New SystemsOfUnits.Converter
            Dim spval1, spval2 As Double
            Dim spci1, spci2 As Integer

            spval1 = SystemsOfUnits.Converter.ConvertToSI(specs("C").SpecUnit, specs("C").SpecValue)
            spci1 = specs("C").ComponentIndex
            spval2 = SystemsOfUnits.Converter.ConvertToSI(specs("R").SpecUnit, specs("R").SpecValue)
            spci2 = specs("R").ComponentIndex

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

            Dim maxF As Double = MathEx.Common.Max(F)

            For i = 0 To ns
                F(i) = F(i) / maxF
                HF(i) = HF(i) / 1000
                L(i) = L(i) / maxF
                V(i) = V(i) / maxF
                LSS(i) = LSS(i) / maxF
                VSS(i) = VSS(i) / maxF
                Q(i) = Q(i) / maxF
            Next

            Dim Sl(ns), Sv(ns) As Double

            For i = 0 To ns
                For j = 0 To nc - 1
                    vc(i)(j) = y(i)(j) * V(i)
                    lc(i)(j) = x(i)(j) * L(i)
                    xc(i)(j) = x(i)(j)
                    yc(i)(j) = y(i)(j)
                    zc(i)(j) = z(i)(j)
                    Tj(i) = T(i)
                Next
                Sv(i) = VSS(i) / V(i)
                Sl(i) = LSS(i) / L(i)
            Next

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
            Dim B As Double = sumF - sumLSS - sumVSS - V(0)

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
            Dim il_err As Double = 0.0#
            Dim il_err_ant As Double = 0.0#

            'independent variables

            Dim VSSj(ns), LSSj(ns), Hv(ns), Hl(ns), Hv0(ns), Hl0(ns) As Double
            Dim sumvkj(ns), sumlkj(ns) As Double
            Dim fxvar((ns + 1) * (2 * nc + 1) - 1) As Double
            Dim xvar((ns + 1) * (2 * nc + 1) - 1), lb((ns + 1) * (2 * nc + 1) - 1), ub((ns + 1) * (2 * nc + 1) - 1) As Double
            Dim dxvar((ns + 1) * (2 * nc + 1) - 1) As Double
            Dim dFdXvar((ns + 1) * (2 * nc + 1) - 1, (ns + 1) * (2 * nc + 1) - 1) As Double
            Dim hes((ns + 1) * (2 * nc + 1) - 1, (ns + 1) * (2 * nc + 1) - 1) As Double

            Dim bx((ns + 1) * (2 * nc + 1) - 1), bx_ant((ns + 1) * (2 * nc + 1) - 1), bxb((ns + 1) * (2 * nc + 1) - 1), bf((ns + 1) * (2 * nc + 1) - 1), bfb((ns + 1) * (2 * nc + 1) - 1), bp((ns + 1) * (2 * nc + 1) - 1), bp_ant((ns + 1) * (2 * nc + 1) - 1) As Double

            For i = 0 To ns
                VSSj(i) = VSS(i)
                LSSj(i) = LSS(i)
            Next

            'solve using IPOPT

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
            _maxF = maxF
            _pp = pp
            _coltype = coltype
            _specs = specs
            _condtype = condt

            _maxT = MathEx.Common.Max(Tj)
            _maxvc = 0.0#
            _maxlc = 0.0#
            For i = 0 To ns
                If MathEx.Common.Max(vc(i)) > _maxvc Then _maxvc = MathEx.Common.Max(vc(i))
                If MathEx.Common.Max(lc(i)) > _maxlc Then _maxlc = MathEx.Common.Max(lc(i))
            Next

            If LowerBound = 0.0# Then LowerBound = 1.0E-20
            If UpperBound = 0.0# Then UpperBound = 1.0E-20

            For i = 0 To ns
                xvar(i * (2 * nc + 1)) = Tj(i) / _maxT
                lb(i * (2 * nc + 1)) = 0.3
                ub(i * (2 * nc + 1)) = 2.0
                For j = 0 To nc - 1
                    xvar(i * (2 * nc + 1) + j + 1) = vc(i)(j) / _maxvc
                    lb(i * (2 * nc + 1) + j + 1) = LowerBound
                    ub(i * (2 * nc + 1) + j + 1) = UpperBound
                    xvar(i * (2 * nc + 1) + j + 1 + nc) = lc(i)(j) / _maxlc
                    lb(i * (2 * nc + 1) + j + 1 + nc) = LowerBound
                    ub(i * (2 * nc + 1) + j + 1 + nc) = UpperBound
                Next
            Next

            grad = False

            'enhance initial estimates with simplex optimization algorithm

            If SimplexPreconditioning Then

                Dim splx As New DotNumerics.Optimization.Simplex

                Dim bvars As New List(Of DotNumerics.Optimization.OptBoundVariable)
                For i = 0 To xvar.Length - 1
                    bvars.Add(New DotNumerics.Optimization.OptBoundVariable(Log(xvar(i)), Log(lb(i)), Log(ub(i))))
                Next

                splx.Tolerance = tol(1)
                splx.MaxFunEvaluations = 1000
                xvar = splx.ComputeMin(Function(xvars() As Double) FunctionValue(xvars), bvars.ToArray).ExpY

                splx = Nothing

            End If

            Dim obj As Double = 0.0#
            Dim status As IpoptReturnCode = IpoptReturnCode.Feasible_Point_Found

            Dim initval(xvar.Length - 1) As Double
            Dim lconstr(xvar.Length - 1) As Double
            Dim uconstr(xvar.Length - 1) As Double

            For i = 0 To xvar.Length - 1
                initval(i) = Log(xvar(i))
                lconstr(i) = Log(lb(i))
                uconstr(i) = Log(ub(i))
            Next

            Dim n As Integer = xvar.Length - 1

            Select Case Solver
                Case OptimizationMethod.Limited_Memory_BGFS
                    Dim variables(n) As OptBoundVariable
                    For i = 0 To n
                        variables(i) = New OptBoundVariable("x" & CStr(i + 1), initval(i), False, lconstr(i), uconstr(i))
                    Next
                    Dim _solver As New L_BFGS_B
                    _solver.Tolerance = tol(1)
                    _solver.MaxFunEvaluations = maxits
                    _solver.AccuracyFactor = 10000000.0
                    initval = _solver.ComputeMin(AddressOf FunctionValue, AddressOf FunctionGradient, variables)
                    _solver = Nothing
                Case OptimizationMethod.Truncated_Newton
                    Dim variables(n) As OptBoundVariable
                    For i = 0 To n
                        variables(i) = New OptBoundVariable("x" & CStr(i + 1), initval(i), False, lconstr(i), uconstr(i))
                    Next
                    Dim _solver As New TruncatedNewton
                    _solver.Tolerance = tol(1)
                    _solver.MaxFunEvaluations = maxits
                    _solver.SearchSeverity = 0.75
                    _solver.MaximunStep = 50
                    initval = _solver.ComputeMin(AddressOf FunctionValue, AddressOf FunctionGradient, variables)
                    _solver = Nothing
                Case OptimizationMethod.Simplex
                    Dim variables(n) As OptBoundVariable
                    For i = 0 To n
                        variables(i) = New OptBoundVariable("x" & CStr(i + 1), initval(i), False, lconstr(i), uconstr(i))
                    Next
                    Dim _solver As New Simplex
                    _solver.Tolerance = tol(1)
                    _solver.MaxFunEvaluations = maxits
                    initval = _solver.ComputeMin(AddressOf FunctionValue, variables)
                    _solver = Nothing
                Case OptimizationMethod.IPOPT
                    Calculator.CheckParallelPInvoke()
                    Using problem As New Ipopt(xvar.Length, lconstr, uconstr, 0, Nothing, Nothing,
                    0, 0, AddressOf eval_f, AddressOf eval_g,
                    AddressOf eval_grad_f, AddressOf eval_jac_g, AddressOf eval_h)
                        problem.AddOption("tol", tol(1))
                        problem.AddOption("max_iter", maxits)
                        problem.AddOption("mu_strategy", "adaptive")
                        problem.AddOption("hessian_approximation", "limited-memory")
                        problem.SetIntermediateCallback(AddressOf intermediate)
                        status = problem.SolveProblem(initval, obj, Nothing, Nothing, Nothing, Nothing)
                    End Using
                Case OptimizationMethod.DifferentialEvolution, OptimizationMethod.GradientDescent, OptimizationMethod.LocalUnimodalSampling,
                    OptimizationMethod.ManyOptimizingLiaisons, OptimizationMethod.Mesh, OptimizationMethod.ParticleSwarm, OptimizationMethod.ParticleSwarmOptimization

                    SwarmOps.Globals.Random = New RandomOps.MersenneTwister()

                    Dim sproblem As New NaphtaliSandholm_ColumnProblem(Me) With {._Dim = initval.Length, ._LB = lconstr, ._UB = uconstr, ._INIT = initval, ._Name = "NS"}
                    sproblem.MaxIterations = maxits
                    sproblem.MinIterations = maxits / 2
                    sproblem.Tolerance = tol(1)
                    Dim opt As SwarmOps.Optimizer = GetSolver(Solver)
                    opt.Problem = sproblem
                    opt.RequireFeasible = True
                    Dim sresult = opt.Optimize(opt.DefaultParameters)

                    initval = sresult.Parameters

            End Select

            xvar = initval.ExpY

            il_err = FunctionValue(xvar.LogY)

            pp.CurrentMaterialStream.Flowsheet.ShowMessage("Naphtali-Sandholm solver: final objective function (error) value = " & il_err, IFlowsheet.MessageType.Information)

            If Abs(il_err) > tol(1) And Not IdealK And Not IdealH Then
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
                    xc(i)(j) = lc(i)(j) / sumlkj(i)
                Next
                For j = 0 To nc - 1
                    yc(i)(j) = vc(i)(j) / sumvkj(i)
                Next
            Next

            ' finished, de-normalize and return arrays
            Dim K(ns, nc - 1) As Object
            For i = 0 To ns
                For j = 0 To nc - 1
                    K(i, j) = _Kval(i)(j)
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
            If condt = Column.condtype.Full_Reflux Then
                Vj(0) = 1.0# - Lj(ns) - sumLSS - sumVSS
                LSSj(0) = 0.0#
            Else
                LSSj(0) = 1.0# - Lj(ns) - sumLSS - sumVSS - Vj(0)
            End If

            For i = 0 To ns
                If Vj(i) <> 0 Then Sv(i) = VSSj(i) / Vj(i) Else Sv(i) = 0
                If Lj(i) <> 0 Then Sl(i) = LSSj(i) / Lj(i) Else Sl(i) = 0
            Next

            Q = _Q.Clone

            For i = 0 To ns
                Lj(i) = sumlkj(i) * maxF
                Vj(i) = sumvkj(i) * maxF
                LSSj(i) = Sl(i) * Lj(i)
                VSSj(i) = Sv(i) * Vj(i)
                F(i) = F(i) * maxF
                L(i) = L(i) * maxF
                V(i) = V(i) * maxF
                LSS(i) = LSS(i) * maxF
                VSS(i) = VSS(i) * maxF
                Q(i) = Q(i) * maxF
            Next

            Return New Object() {Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ec, il_err, ic, el_err, dFdXvar}

        End Function

        Public Function eval_f(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByRef obj_value As Double) As Boolean
            Dim fval As Double = FunctionValue(x)
            obj_value = fval
            Return True
        End Function

        Public Function eval_grad_f(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByRef grad_f As Double()) As Boolean
            Dim g As Double() = FunctionGradient(x)
            grad_f = g
            Return True
        End Function

        Public Function eval_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByRef g As Double()) As Boolean
            Return True
        End Function

        Public Function eval_jac_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByVal nele_jac As Integer, ByRef iRow As Integer(),
         ByRef jCol As Integer(), ByRef values As Double()) As Boolean
            Return False
        End Function

        Public Function eval_h(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal obj_factor As Double, ByVal m As Integer, ByVal lambda As Double(),
         ByVal new_lambda As Boolean, ByVal nele_hess As Integer, ByRef iRow As Integer(), ByRef jCol As Integer(), ByRef values As Double()) As Boolean
            Return False
        End Function

        Public Function intermediate(ByVal alg_mod As IpoptAlgorithmMode, ByVal iter_count As Integer, ByVal obj_value As Double,
                             ByVal inf_pr As Double, ByVal inf_du As Double, ByVal mu As Double,
                             ByVal d_norm As Double, ByVal regularization_size As Double, ByVal alpha_du As Double,
                             ByVal alpha_pr As Double, ByVal ls_trials As Integer) As Boolean
            '_pp.CurrentMaterialStream.Flowsheet.ShowMessage("Naphtali-Sandholm solver iteration #" & iter_count & ": current objective function (error) value = " & obj_value, IFlowsheet.MessageType.Information)
            Return True
        End Function

    End Class

    Public Class Russell_ColumnProblem

        Inherits SwarmOps.Problem

        Public _Dim As Integer, _LB(), _UB(), _INIT() As Double, _Name As String

        Private _gf As RussellMethod
        Private _fit As Double

        Sub New(gf As RussellMethod)
            _gf = gf
        End Sub

        Public Overrides ReadOnly Property Dimensionality As Integer
            Get
                Return _Dim
            End Get
        End Property

        Public Overrides ReadOnly Property LowerBound As Double()
            Get
                Return _LB
            End Get
        End Property

        Public Overrides ReadOnly Property LowerInit As Double()
            Get
                Return _INIT
            End Get
        End Property
        Public Overrides ReadOnly Property UpperInit As Double()
            Get
                Return _INIT
            End Get
        End Property

        Public Overrides ReadOnly Property MinFitness As Double
            Get
                Return 0.0#
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                Return _Name
            End Get
        End Property

        Public Overrides ReadOnly Property UpperBound As Double()
            Get
                Return _UB
            End Get
        End Property

        Public Overrides ReadOnly Property HasGradient As Boolean
            Get
                Return True
            End Get
        End Property

        Public Overrides Function Gradient(x() As Double, ByRef v() As Double) As Integer

            v = _gf.FunctionGradient(x)

            Return 0

        End Function

        Public Overrides Function Fitness(parameters() As Double) As Double

            Return _gf.FunctionValue(parameters)

        End Function

        Public Overrides Function [Continue](iterations As Integer, fitness As Double, feasible As Boolean) As Boolean
            '_gf._pp.CurrentMaterialStream.Flowsheet.ShowMessage("Russell Inside-Out solver iteration #" & iterations & ": current objective function (error) value = " & fitness, IFlowsheet.MessageType.Information)
            Return MyBase.[Continue](iterations, fitness, feasible)
        End Function

        Public Overrides ReadOnly Property AcceptableFitness As Double
            Get
                Return 0.01
            End Get
        End Property

        Public Overrides ReadOnly Property MaxFitness As Double
            Get
                Return 10000
            End Get
        End Property

    End Class

    Public Class NaphtaliSandholm_ColumnProblem

        Inherits SwarmOps.Problem

        Public _Dim As Integer, _LB(), _UB(), _INIT() As Double, _Name As String

        Private _gf As NaphtaliSandholmMethod
        Private _fit As Double

        Sub New(gf As NaphtaliSandholmMethod)
            _gf = gf
        End Sub

        Public Overrides ReadOnly Property Dimensionality As Integer
            Get
                Return _Dim
            End Get
        End Property

        Public Overrides ReadOnly Property LowerBound As Double()
            Get
                Return _LB
            End Get
        End Property

        Public Overrides ReadOnly Property LowerInit As Double()
            Get
                Return _INIT
            End Get
        End Property
        Public Overrides ReadOnly Property UpperInit As Double()
            Get
                Return _INIT
            End Get
        End Property

        Public Overrides ReadOnly Property MinFitness As Double
            Get
                Return 0.0#
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                Return _Name
            End Get
        End Property

        Public Overrides ReadOnly Property UpperBound As Double()
            Get
                Return _UB
            End Get
        End Property

        Public Overrides ReadOnly Property HasGradient As Boolean
            Get
                Return True
            End Get
        End Property

        Public Overrides Function Gradient(x() As Double, ByRef v() As Double) As Integer

            v = _gf.FunctionGradient(x)

            Return 0

        End Function

        Public Overrides Function Fitness(parameters() As Double) As Double

            Return _gf.FunctionValue(parameters)

        End Function

        Public Overrides Function [Continue](iterations As Integer, fitness As Double, feasible As Boolean) As Boolean
            '_gf._pp.CurrentMaterialStream.Flowsheet.ShowMessage("Naphtali-Sandholm solver iteration #" & iterations & ": current objective function (error) value = " & fitness, IFlowsheet.MessageType.Information)
            Return MyBase.[Continue](iterations, fitness, feasible)
        End Function

        Public Overrides ReadOnly Property AcceptableFitness As Double
            Get
                Return 0.01
            End Get
        End Property

        Public Overrides ReadOnly Property MaxFitness As Double
            Get
                Return 10000
            End Get
        End Property

    End Class

End Namespace
