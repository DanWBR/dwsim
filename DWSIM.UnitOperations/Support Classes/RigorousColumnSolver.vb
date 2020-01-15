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

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "TDMASolve", "Tridiagonal Matrix Algorithm", "Tomich TDM Solver", True)

            IObj?.Paragraphs.Add("The key to the BP and SR tearing procedures is the tridiagonal matrix, which results from a modified form of the M equations, when they are torn from the other equations by selecting Tj and Vj as the tear variables, leaving the modified M equations linear in the unknown liquid mole fractions.")

            IObj?.Paragraphs.Add("This set of equations, one for each component, is solved by a modified Gaussian–elimination algorithm due to Thomas as applied by Wang and Henke. Equations for calculating y and L are partitioned from the other equations. The result for each component, i, and each stage, j, is as follows, where the i subscripts have been dropped from the B, C, and D terms.")

            IObj?.Paragraphs.Add("<math>A_jx_{i,j-1}+B_jx_{i,j}+C_jx_{i,j+1}=D_j</math>")

            IObj?.Paragraphs.Add("where")

            IObj?.Paragraphs.Add("<math>A_j = V_j+\sum\limits_{m=1}^{j-1} (F_m-W_m-U_m)-V_1</math>")

            IObj?.Paragraphs.Add("<math>B_j=-[V_{j+1}+\sum\limits_{m+1}^j(F_m-W_m-U_m)-V_1+U_j+(V_j+W_j)K_{i,j}], 1\leq j\leq N</math>")

            IObj?.Paragraphs.Add("<math>C_j=V_{j+1}K_{i,j+1}, 1 \leq j \leq N-1</math>")

            IObj?.Paragraphs.Add("<math>D_j=-F_jz_{i,j}, 1 \leq j \leq N</math>")

            IObj?.Paragraphs.Add("with <mi>x_{i,0}=0</mi>; <mi>V_{N+1}=0</mi>; <mi>W_1 = 0</mi>, and <mi>U_N=0</mi>, as indicated in Figure 10.3. If the modified M equations are grouped by component, they can be partitioned by writing them as a series of separate tridiagonal-matrix equations, one for each component, where the output variable for each matrix equation is xi over the entire N-stage cascade.")

            IObj?.Paragraphs.Add("Constants Bj and Cj for each component depend only on tear variables T and V if K-values are composition-independent. If not, previous iteration compositions may be used to estimate K-values.")

            IObj?.Paragraphs.Add("The Thomas algorithm for solving the linearized equation set is a Gaussian–elimination procedure involving forward elimination starting from stage 1 and working toward stage N to finally isolate <mi>x_{i,N}</mi>. Other values of <mi>x_{i,j}</mi> are then obtained, starting with <mi>x_{i,N-1}</mi> by backward substitution.")

            IObj?.Paragraphs.Add("The Thomas algorithm avoids buildup of computer truncation errors because none of the steps involves subtraction of nearly equal quantities. Furthermore, computed values of xi,j are almost always positive. The algorithmis superior to alternative matrix-inversion routines. A modified Thomas algorithm for difficult cases is given by Boston and Sullivan [9]. Such cases can occur for columns with large numbers of equilibrium stages and components whose absorption factors, <mi>A=L/KV</mi>, are less than unity in one section and greater than unity in another.")

            IObj?.Paragraphs.Add("<h2>Input Parameters</h2>")

            IObj?.Paragraphs.Add(String.Format("A: {0}", a.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("B: {0}", b.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("C: {0}", c.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("D: {0}", d.ToMathArrayString))

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

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("x: {0}", x.ToMathArrayString))

            IObj?.Close()

            Return x

        End Function

    End Class

    <System.Serializable()> Public Class RussellMethod

        Dim _IObj As Inspector.InspectorItem

        Sub New()

        End Sub

        Private Function CalcKbj1(ByVal ns As Integer, ByVal nc As Integer, ByVal K(,) As Double,
                                        ByVal z()() As Double, ByVal y()() As Double, ByVal T() As Double,
                                        ByVal P() As Double, ByRef pp As PropertyPackages.PropertyPackage) As Double()

            Dim i, j As Integer

            Dim Kbj1(ns) As Double

            For i = 0 To ns
                Kbj1(i) = K(i, 0)
                For j = 1 To nc - 1
                    If Abs(K(i, j) - 1) < Abs(Kbj1(i) - 1) And z(i)(j) <> 0 Then Kbj1(i) = K(i, j)
                Next
            Next

            Return Kbj1

        End Function

        Private Function CalcKbj2(ByVal ns As Integer, ByVal nc As Integer, ByVal K(,) As Double,
                                      ByVal z()() As Double, ByVal y()() As Double, ByVal T() As Double,
                                      ByVal P() As Double, ByRef pp As PropertyPackages.PropertyPackage) As Double()

            Dim i, j As Integer

            Dim Kbj1(ns) As Double
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
        Dim _Kbj As Double()
        Dim _rr, _Sb, _maxF, _maxtchange As Double
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

            _IObj?.SetCurrent

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "FunctionValue", "Inside-Out (IO) Method Modified MESH Equations Calculator", "Russell IO Method for Distillation, Absorption and Stripping", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add(String.Format("Input Variables: {0}", x.ToMathArrayString))

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
                IObj?.SetCurrent()
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
                If Abs(_Tj(i) - _Tj_ant(i)) > _maxtchange Or Double.IsNaN(_Tj(i)) Or Double.IsInfinity(_Tj(i)) Then
                    'switch to a bubble point temperature calculation...
                    IObj?.SetCurrent()
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
                If Abs(_Tj(i) - _Tj_ant(i)) > _maxtchange Then
                    _Tj(i) = _Tj_ant(i) + Sign(_Tj(i) - _Tj_ant(i)) * _maxtchange
                End If
            Next

            'step7

            'calculate enthalpies

            Dim Hv(_ns), Hl(_ns), Hidv(_ns), Hidl(_ns), DHv(_ns), DHl(_ns) As Double

            For i = 0 To _ns
                IObj?.SetCurrent()
                Hidv(i) = _pp.RET_Hid(298.15, _Tj(i), _yc(i))
                IObj?.SetCurrent()
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

            IObj?.Paragraphs.Add(String.Format("MESH Equation Deviations: {0}", errors.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("Total Error: {0}", errors.AbsSqrSumY))

            IObj?.Close()

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

        Public Function Solve(ByVal nc As Integer, ByVal ns As Integer, ByVal maxits As Integer,
                                ByVal tol As Double(), ByVal F As Double(), ByVal V As Double(),
                                ByVal Q As Double(), ByVal L As Double(),
                                ByVal VSS As Double(), ByVal LSS As Double(), ByVal Kval()() As Double,
                                ByVal x()() As Double, ByVal y()() As Double, ByVal z()() As Double,
                                ByVal fc()() As Double,
                                ByVal HF As Double(), ByVal T As Double(), ByVal P As Double(),
                                ByVal condt As DistillationColumn.condtype,
                                ByVal eff() As Double,
                                ByVal AdjustSb As Boolean,
                                ByVal coltype As Column.ColType, ByVal KbjWA As Boolean,
                                ByVal pp As PropertyPackages.PropertyPackage,
                                ByVal specs As Dictionary(Of String, SepOps.ColumnSpec),
                                ByVal epsilon As Double,
                                ByVal Solver As OptimizationMethod,
                                ByVal LowerBound As Double, ByVal UpperBound As Double,
                                ByVal IdealK As Boolean, ByVal IdealH As Boolean,
                                ByVal MaxTChange As Double,
                                Optional ByVal llex As Boolean = False) As Object

            _maxtchange = MaxTChange

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Solve", "Inside-Out (IO) Method", "Russell Inside-Out (IO) Method for Distillation Columns", True)

            IObj?.Paragraphs.Add("In the BP, SR, and NR methods, the major computational effort is expended in calculating K-values and enthalpies when rigorous thermodynamic-property models are utilized, because property calculations are made at each iteration. Furthermore, at each iteration, derivatives are required of: (1) all properties with respect to temperature and compositions of both phases for the NR method; (2) K-values with respect to temperature for the BP method, unless Muller’s method is used, and (3) vapor and liquid enthalpies with respect to temperature for the SR method.")

            IObj?.Paragraphs.Add("In 1974, Boston and Sullivan presented an algorithm designed to reduce the time spent computing thermodynamic properties when making column calculations. Two sets of thermodynamic-property models are employed: (1) a simple, approximate, empirical set used frequently to converge inner-loop calculations, and (2) a rigorous set used less often in the outer loop. The MESH equations are always solved in the inner loop with the approximate set. The parameters in the empirical equations for the inner-loop set are updated only infrequently in the outer loop using the rigorous equations. The distinguishing Boston–Sullivan feature is the inner and outer loops, hence the name inside-out method. Another feature of the inside-out method is the choice of iteration variables. For the NR method, the iteration variables are <mi>l_{ij}</mi>, <mi>y_{ij}</mi>, and <mi>T_{j}</mi>. For the BP and SR methods, the choice is <mi>x_{ij}</mi>, <mi>y_{ij}</mi>, <mi>T_{j}</mi>, <mi>L_{j}</mi>, and <mi>V_{j}</mi>. For the inside-out method, the iteration variables for the outer loop are the parameters in the approximate equations for the thermodynamic properties. The iteration variables for the inner loop are related to stripping factors, <mi>S_{ij}=K_{i,j}V_j/L_j</mi>.")

            IObj?.Paragraphs.Add("The inside-out method takes advantage of the following characteristics of the iterative calculations:")

            IObj?.Paragraphs.Add("1. Component relative volatilities vary much less than component K-values.")

            IObj?.Paragraphs.Add("2. Enthalpy of vaporization varies less than phase enthalpies.")

            IObj?.Paragraphs.Add("3. Component stripping factors combine effects of temperature and liquid and vapor flows at each stage. ")

            IObj?.Paragraphs.Add("The inner loop of the inside-out method uses relative volatility, energy, and stripping factors to improve stability and reduce computing time. A widely used implementation is that of Russell, which is described here together with further refinements suggested and tested by Jelinek.")

            IObj?.Paragraphs.Add("<h3>MESH Equations for Inside-Out Method</h3>")

            IObj?.Paragraphs.Add("As with the BP, SR, and NR methods, the equilibrium-stage model is employed. The form of the equations is similar to the NR method in that component flow rates are utilized. However, in addition, the following innerloop variables are defined:")

            IObj?.Paragraphs.Add("<m>\alpha _{i,j}=K_{i,j}/K_{b,j}</m>")

            IObj?.Paragraphs.Add("<m>S_{b,j}=K_{b,j}V_j/L_j</m>")

            IObj?.Paragraphs.Add("<m>R_{L_j}=1+U_j/L_j</m>")

            IObj?.Paragraphs.Add("<m>R_{V_j}=1+W_j/V_j</m>")

            IObj?.Paragraphs.Add("where <mi>K_b</mi> is the K-value for a base or hypothetical reference component, <mi>S_{b,j}</mi> is the stripping factor for the base component, <mi>R_{L_j}</mi> is a liquid-phase withdrawal factor, and <mi>R_{V_j}</mi> is a vapor-phase withdrawal factor. For stages without sidestreams, <mi>R_{L_j}</mi> and <mi>R_{V_j}</mi> reduce to 1. The MESH equations become as follows:")

            IObj?.Paragraphs.Add("<h4>Phase Equilibria</h4>")

            IObj?.Paragraphs.Add("<m>v_{i,j}=\alpha _{i,j}S_{b,j}l_{i,j}, i=1 to C, j = 1 to N</m>")

            IObj?.Paragraphs.Add("<h4>Component Material Balance</h4>")

            IObj?.Paragraphs.Add("<m>l_{i,j-1}-(R_{L_j}+\alpha _{i,j}S_{b,j}R_{V_j})l_{i,j}+(\alpha _{i,j+1}S_{b,j+1})l_{i,j+1}=-f_{i,j}, i=1 to C, j = 1 to N</m>")

            IObj?.Paragraphs.Add("<h4>Energy Balance</h4>")

            IObj?.Paragraphs.Add("<m>H_j=h_{L_j}R_{L_j}L_j+h_{V_j}R_{V_j}V_j-h_{L_{j-1}}-h_{V_{j+1}}V_{j+1}-h_{F_j}F_j-Q_j=0, j = 1 to N</m>")

            IObj?.Paragraphs.Add("where <mi>S_{i,j}=\alpha _{i,j}S_{b,j}</mi>.")

            IObj?.Paragraphs.Add("In addition, discrepancy functions can be added to the MESH equations to permit any reasonable set of product specifications.")

            IObj?.Paragraphs.Add("<h3>Rigorous and Complex Thermodynamic-Property Models</h3>")

            IObj?.Paragraphs.Add("The complex thermodynamic models referred to in Figure 10.30 can include any of the models discussed in Chapter 2, including those based on P–y–T equations of state and those based on free-energy models for liquid-phase activity coefficients. These generate parameters in the approximate thermodynamic-property models of the form")

            IObj?.Paragraphs.Add("<m>K_{i,j}=K_{i,j}\left\{P_j,T_j,x_j,y_j\right\}</m>")

            IObj?.Paragraphs.Add("<m>h_{V_j}=h_{V_j}\left\{P_j,T_j,x_j\right\}</m>")

            IObj?.Paragraphs.Add("<m>h_{L_j}=h_{L_j}\left\{P_j,T_j,x_j\right\}</m>")

            IObj?.Paragraphs.Add("<h3>Approximate Thermodynamic-Property Models</h3>")

            IObj?.Paragraphs.Add("The approximate models in the inside-out method are designed to facilitate calculation of stage temperatures and stripping factors.")

            IObj?.Paragraphs.Add("<h4>K-values</h4>")

            IObj?.Paragraphs.Add("The approximate K-value model of Russell and Jelinek, which differs slightly from that of Boston and Sullivan and originated from a proposal in Robinson and Gilliland,")

            IObj?.Paragraphs.Add("<m>K_{b,j}=\exp (A_j-B_j/T_j)</m>")

            IObj?.Paragraphs.Add("Either a feed component or a hypothetical reference component can be selected as the base, b, with the latter preferred, and determined from a vapor-composition weighting using the following relations:")

            IObj?.Paragraphs.Add("<m>K_{b,j}=\exp \left(\sum\limits_{i}{w_{i,j}\ln K_{i,j}} \right) </m>")

            IObj?.Paragraphs.Add("where <mi>w_{i,j}</mi> are weighting functions given by")

            IObj?.Paragraphs.Add("<m>w_{i,j}=\frac{y_{i,j\left[\partial \ln K_{i,j}/\partial (1/T)\right] }}{\sum\limits_{i}{y_{i,j}\left[\partial \ln K_{i,j}/\partial (1/T)\right] }} </m>")

            IObj?.Paragraphs.Add("<h4>Enthalpies</h4>")

            IObj?.Paragraphs.Add("Boston and Sullivan and Russell employ the same approximate enthalpy models. Jelinek does not use approximate enthalpy models, because the additional complexity involved in the use of two enthalpy models may not always be justified to the extent that approximate and rigorous K-value models are justified. ")

            IObj?.Paragraphs.Add("<h3>Inside-Out Algorithm</h3>")

            IObj?.Paragraphs.Add("The inside-out algorithm of Russell involves an initialization procedure, inner-loop iterations, and outer-loop iterations.")

            IObj?.Paragraphs.Add("<h4>Initialization Procedure</h4>")

            IObj?.Paragraphs.Add("First, it is necessary to provide reasonably good estimates of all stage values of <mi>x_{i,j}</mi>, <mi>y_{i,j}</mi>, Tj, Vj, and Lj. Boston and Sullivan suggest the following procedure:")

            IObj?.Paragraphs.Add("1. Specify the number of theoretical stages, conditions of all feeds, feed-stage locations, and pressure profile.")

            IObj?.Paragraphs.Add("2. Specify stage locations for each product withdrawal (including sidestreams) and for each heat exchanger.")

            IObj?.Paragraphs.Add("3. Provide an additional specification for each product and each intermediate heat exchanger. ")

            IObj?.Paragraphs.Add("4. If not specified, estimate each product-withdrawal rate, and estimate each value of Vj. Obtain values of Lj from the total material-balance equation.")

            IObj?.Paragraphs.Add("5. Estimate an initial temperature profile, Tj, by combining all feed streams (composite feed) and determining bubble- and dew-point temperatures at average column pressure. The dew-point temperature is the top-stage temperature, T1, whereas the bubble-point temperature is the bottom-stage temperature, TN. Intermediate stage temperatures are estimated by interpolation.")

            IObj?.Paragraphs.Add("6. Flash the composite feed isothermally at the average column pressure and temperature. The resulting vapor and liquid compositions, yi and xi, are the estimated stage compositions.")

            IObj?.Paragraphs.Add("7. With the initial estimates from steps 1 through 6, use the complex thermodynamic-property correlation to determine values of the stagewise outside-loop K and h parameters Aj, Bj, ai,j, <mi>b_{i,j}</mi>, cj, dj, ej, fj, <mi>K_{i,j}</mi>, and <mi>\alpha _{i,j}</mi> of the approximate models.")

            IObj?.Paragraphs.Add("8. Compute initial values of <mi>S_{b,j}</mi>, <mi>R_{L_j}</mi>, and <mi>R_{V_j}</mi>.")

            IObj?.Paragraphs.Add("<h4>Inner-Loop Calculation Sequence</h4>")

            IObj?.Paragraphs.Add("An iterative sequence of inner-loop calculations begins with values for the outside-loop parameters listed in step 7, obtained initially from the initialization procedure and later from outer-loop calculations, using results from the inner loop.")

            IObj?.Paragraphs.Add("9. Compute component liquid flow rates, <mi>l_{i,j}</mi>, from the set of N equations for each of the C components by the tridiagonal-matrix algorithm.")

            IObj?.Paragraphs.Add("10. Compute component vapor flows, <mi>y_{i,j}</mi>.")

            IObj?.Paragraphs.Add("11. Compute a revised set of flow rates, Vj and Lj, from the component flow rates.")

            IObj?.Paragraphs.Add("12. To calculate a revised set of stage temperatures, Tj, compute a set of xi values for each stage, then a revised set of <mi>K_{b,j}</mi> values from a combination of the bubble-point equation, which gives")

            IObj?.Paragraphs.Add("<m>K_{b,j}=\frac{1}{\sum\limits_{i=1}^{C}{(\alpha _{i,j}x_{i,j})} } </m>")

            IObj?.Paragraphs.Add("From this new set of <mi>K_{b,j}</mi> values, compute a set of stage temperatures:")

            IObj?.Paragraphs.Add("<m>T_j=\frac{B_j}{A_j-\ln K_{b,j}}</m>")

            IObj?.Paragraphs.Add("At this point in the inner-loop iterative sequence, there is a revised set of <mi>y_{i,j}</mi>, <mi>l_{i,j}</mi>, and Tj, which satisfy the component material-balance and phase-equilibria equations for the estimated properties. However, these values do not satisfy the energy-balance and specification equations unless the estimated base-component stripping factors and productwithdrawal rates are correct.")

            IObj?.Paragraphs.Add("13. Select inner-loop iteration variables as")

            IObj?.Paragraphs.Add("<m>\ln S_{b,j}=\ln (K_{b,j}V_j/L_j)</m>")

            IObj?.Paragraphs.Add("together with any other iteration variables. If the reflux ratio (L/D) and bottoms flow rate (B) are specified rather than the two duties (which is the more common situation), the two discrepancy functions D1 and D2 are added:")

            IObj?.Paragraphs.Add("<m>D_1=L_1-(L/D)V_1=0</m>")

            IObj?.Paragraphs.Add("<m>D_2=L_N-B=0</m>")

            IObj?.Paragraphs.Add("For each sidestream, a sidestream-withdrawal factor is added as an inner-loop iteration variable, e.g., ln(Uj/Lj) and ln(Wj/Vj), together with a specification on purity or some other variable.")

            IObj?.Paragraphs.Add("14. Compute stream enthalpies.")

            IObj?.Paragraphs.Add("15. Compute normalized discrepancies of Hj, D1, D2, etc., from the energy balances but compute Q1 from H1, and QN from HN where appropriate.")

            IObj?.Paragraphs.Add("16. Compute the Jacobian of partial derivatives of Hj, D1, D2, etc., with respect to the iteration variables, by perturbation of each iteration variable and recalculation of the discrepancies through steps 9 to 15, numerically or by differentiation.")

            IObj?.Paragraphs.Add("17. Compute corrections to the inner-loop iteration variables by a NR iteration of the type discussed for the SR and NR methods.")

            IObj?.Paragraphs.Add("18. Compute new values of the iteration variables from the sum of the previous values and the corrections with (10-66), using damping if necessary to reduce the sum of squares of the normalized discrepancies.")

            IObj?.Paragraphs.Add("19. Check whether the sum-of-squares is sufficiently small. If so, proceed to the outer-loop calculation procedure given next. If not, repeat steps 15 to 18 using the latest iteration variables. For any subsequent cycles through steps 15 to 18, Russell uses Broyden updates to avoid reestimation of the Jacobian partial derivatives, whereas Jelinek recommends the standard NR method of recalculating the partial derivatives for each inner-loop iteration.")

            IObj?.Paragraphs.Add("20. Upon convergence of steps 15 to 19, steps 9 through 12 will have produced an improved set of primitive variables <mi>x_{i,j}</mi>, <mi>y_{i,j}</mi>, <mi>l_{i,j}</mi>, Tj, Vj, and Lj. The values of these variables are not correct until the approximate thermodynamic properties are in agreement with the properties from the rigorous models. The primitive variables are input to the outer-loop calculations to bring the approximate and complex models into successively better agreement.")

            IObj?.Paragraphs.Add("<h4>Outer-Loop Calculation Sequence</h4>")

            IObj?.Paragraphs.Add("Each outer loop proceeds as follows:")

            IObj?.Paragraphs.Add("21. Using the values of the primitive variables from step 20, compute relative volatilities and stream enthalpies from the complex thermodynamic models. If they are in close agreement with previous values used to initiate a set of inner-loop iterations, both outer- and inner-loop iterations are converged, and the problem is solved. If not, proceed to step 22.")

            IObj?.Paragraphs.Add("22. Determine values of the stagewise outside-loop K and h parameters of the approximate models from the complex models, as in initialization step 7.")

            IObj?.Paragraphs.Add("23. Compute values of Sb,j, RLj, and RVj, as in initialization step 8.")

            IObj?.Paragraphs.Add("24. Repeat the inner-loop calculation of Steps 9–20. Although convergence of the inside-out method is not guaranteed, for most problems, the method is robust and rapid. Convergence difficulties arise because of poor initial estimates, which result in negative or zero flow rates at certain locations in the column. To counteract this tendency, all component stripping factors use a scalar multiplier, Sb, called a base stripping factor, to give")

            IObj?.Paragraphs.Add("<m>S_{i,j}=S_b\alpha _{i,j}S_{b,j}</m>")

            IObj?.Paragraphs.Add("The value of Sb is initially chosen to force the results of the initialization procedure to give a reasonable distribution of component flows throughout the column. Russell recommends that Sb be chosen only once, but Boston and Sullivan compute a new Sb for each new set of <mi>S_{b,j}</mi> values.")

            IObj?.Paragraphs.Add("For highly nonideal-liquid mixtures, use of the inside-out method may lead to difficulties, and the NR method should be tried. If the NR method also fails to converge, relaxation or continuation methods are usually successful, but computing time may be an order of magnitude longer than that for similar problems converged successfully with the inside-out method.")

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

            ik = IdealK
            ih = IdealH

            If ik Or ih Then
                _ppr = New PropertyPackages.RaoultPropertyPackage
                _ppr.Flowsheet = pp.Flowsheet
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

            Dim Kbj(ns), Kbj_ant(ns) As Double
            Dim K(ns, nc - 1), K_ant(ns, nc - 1), K2j(ns, nc - 1) As Double

            Dim Kw1(ns)(), Kw2(ns)() As Object
            Dim wi(ns, nc - 1), ti(ns, nc - 1), sumwi(ns), sumti(ns) As Double
            For i = 0 To ns
                Array.Resize(Kw1(i), nc)
                Array.Resize(Kw2(i), nc)
            Next

            Dim tmp0 As Object = Nothing

            For i = 0 To ns
                IObj?.SetCurrent()
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

            IObj?.SetCurrent()
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

            Dim Kbj1(ns), Kbj2(ns) As Double
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
                    IObj?.SetCurrent()
                    If llextr Then
                        K2(i) = _ppr.DW_CalcKvalue(x(i), y(i), Tj2(i), P(i), "LL")
                        IObj?.SetCurrent()
                        Hv1(i) = _ppr.DW_CalcEnthalpyDeparture(y(i), Tj1(i), P(i), PropertyPackages.State.Liquid)
                        IObj?.SetCurrent()
                        Hv2(i) = _ppr.DW_CalcEnthalpyDeparture(y(i), Tj2(i), P(i), PropertyPackages.State.Liquid)
                    Else
                        K2(i) = _ppr.DW_CalcKvalue(x(i), y(i), Tj2(i), P(i))
                        IObj?.SetCurrent()
                        Hv1(i) = _ppr.DW_CalcEnthalpyDeparture(y(i), Tj1(i), P(i), PropertyPackages.State.Vapor)
                        IObj?.SetCurrent()
                        Hv2(i) = _ppr.DW_CalcEnthalpyDeparture(y(i), Tj2(i), P(i), PropertyPackages.State.Vapor)
                    End If
                    IObj?.SetCurrent()
                    Hl1(i) = _ppr.DW_CalcEnthalpyDeparture(x(i), Tj1(i), P(i), PropertyPackages.State.Liquid)
                    IObj?.SetCurrent()
                    Hl2(i) = _ppr.DW_CalcEnthalpyDeparture(x(i), Tj2(i), P(i), PropertyPackages.State.Liquid)
                Else
                    IObj?.SetCurrent()
                    If llextr Then
                        K2(i) = pp.DW_CalcKvalue(x(i), y(i), Tj2(i), P(i), "LL")
                        IObj?.SetCurrent()
                        Hv1(i) = pp.DW_CalcEnthalpyDeparture(y(i), Tj1(i), P(i), PropertyPackages.State.Liquid)
                        IObj?.SetCurrent()
                        Hv2(i) = pp.DW_CalcEnthalpyDeparture(y(i), Tj2(i), P(i), PropertyPackages.State.Liquid)
                    Else
                        K2(i) = pp.DW_CalcKvalue(x(i), y(i), Tj2(i), P(i))
                        IObj?.SetCurrent()
                        Hv1(i) = pp.DW_CalcEnthalpyDeparture(y(i), Tj1(i), P(i), PropertyPackages.State.Vapor)
                        IObj?.SetCurrent()
                        Hv2(i) = pp.DW_CalcEnthalpyDeparture(y(i), Tj2(i), P(i), PropertyPackages.State.Vapor)
                    End If
                    IObj?.SetCurrent()
                    Hl1(i) = pp.DW_CalcEnthalpyDeparture(x(i), Tj1(i), P(i), PropertyPackages.State.Liquid)
                    IObj?.SetCurrent()
                    Hl2(i) = pp.DW_CalcEnthalpyDeparture(x(i), Tj2(i), P(i), PropertyPackages.State.Liquid)
                End If
                For j = 0 To nc - 1
                    K2j(i, j) = K2(i)(j)
                    If Double.IsNaN(K2(i)(j)) Or Double.IsInfinity(K2(i)(j)) Then K2(i)(j) = pp.AUX_PVAPi(j, T(i)) / P(i)
                Next
            Next

            IObj?.SetCurrent()
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

            IObj?.Paragraphs.Add("Initializing Model Variables...")

            IObj?.Paragraphs.Add(String.Format("Kbj: {0}", Kbj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("alpha ij: {0}", alpha.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("A: {0}", Aj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("B: {0}", Bj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("C: {0}", Cj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("D: {0}", Dj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("E: {0}", Ej.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("F: {0}", Fj.ToMathArrayString))

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

            IObj?.Paragraphs.Add(String.Format("Sb: {0}", Sbj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Rl: {0}", Rlj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Rv: {0}", Rvj.ToMathArrayString))

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

                IObj?.SetCurrent()

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj2, "", "Solve", "IO External Loop Iteration", "Inside-Out External Loop Convergence Iteration Step")

                IObj2?.Paragraphs.Add(String.Format("This is the IO external convergence loop iteration #{0}.", ec))

                IObj2?.SetCurrent()

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

                _IObj = IObj2

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
                        IObj2?.SetCurrent()
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

                IObj2?.SetCurrent()
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
                        IObj2?.SetCurrent()
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
                        IObj2?.SetCurrent()
                        'enthalpies
                        If ih Then
                            If llextr Then
                                Hv1(i) = _ppr.DW_CalcEnthalpyDeparture(yc(i), Tj1(i), P(i), PropertyPackages.State.Liquid)
                                IObj2?.SetCurrent()
                                Hv2(i) = _ppr.DW_CalcEnthalpyDeparture(yc(i), Tj2(i), P(i), PropertyPackages.State.Liquid)
                            Else
                                Hv1(i) = _ppr.DW_CalcEnthalpyDeparture(yc(i), Tj1(i), P(i), PropertyPackages.State.Vapor)
                                IObj2?.SetCurrent()
                                Hv2(i) = _ppr.DW_CalcEnthalpyDeparture(yc(i), Tj2(i), P(i), PropertyPackages.State.Vapor)
                            End If
                            IObj2?.SetCurrent()
                            Hl1(i) = _ppr.DW_CalcEnthalpyDeparture(xc(i), Tj1(i), P(i), PropertyPackages.State.Liquid)
                            IObj2?.SetCurrent()
                            Hl2(i) = _ppr.DW_CalcEnthalpyDeparture(xc(i), Tj2(i), P(i), PropertyPackages.State.Liquid)
                        Else
                            If llextr Then
                                Hv1(i) = pp.DW_CalcEnthalpyDeparture(yc(i), Tj1(i), P(i), PropertyPackages.State.Liquid)
                                IObj2?.SetCurrent()
                                Hv2(i) = pp.DW_CalcEnthalpyDeparture(yc(i), Tj2(i), P(i), PropertyPackages.State.Liquid)
                            Else
                                Hv1(i) = pp.DW_CalcEnthalpyDeparture(yc(i), Tj1(i), P(i), PropertyPackages.State.Vapor)
                                IObj2?.SetCurrent()
                                Hv2(i) = pp.DW_CalcEnthalpyDeparture(yc(i), Tj2(i), P(i), PropertyPackages.State.Vapor)
                            End If
                            IObj2?.SetCurrent()
                            Hl1(i) = pp.DW_CalcEnthalpyDeparture(xc(i), Tj1(i), P(i), PropertyPackages.State.Liquid)
                            IObj2?.SetCurrent()
                            Hl2(i) = pp.DW_CalcEnthalpyDeparture(xc(i), Tj2(i), P(i), PropertyPackages.State.Liquid)
                        End If

                    Next
                End If

                IObj2?.SetCurrent()
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

                IObj2?.Paragraphs.Add("Updating Model Variables...")

                IObj2?.Paragraphs.Add(String.Format("Kbj: {0}", Kbj.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("alpha ij: {0}", alpha.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("A: {0}", Aj.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("B: {0}", Bj.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("C: {0}", Cj.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("D: {0}", Dj.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("E: {0}", Ej.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("F: {0}", Fj.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("Sb: {0}", Sbj.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("Rl: {0}", Rlj.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("Rv: {0}", Rvj.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("External Loop error: {0}", el_err))

                ec += 1

                If ec >= maxits And Not IdealK And Not IdealH Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCMaxIterationsReached"))
                If Double.IsNaN(el_err) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))

                If AdjustSb Then SbOK = False
                Sb = 1

                pp.CurrentMaterialStream.Flowsheet.CheckStatus()

                pp.CurrentMaterialStream.Flowsheet.ShowMessage("Inside-Out solver: outer loop error value = " & el_err, IFlowsheet.MessageType.Information)

                IObj2?.Close()

            Loop Until Abs(el_err) < tol(1) And Abs(il_err) < tol(0)

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

            For Each Ki In K.ToJaggedArray
                If pp.AUX_CheckTrivial(Ki) Then
                    IObj?.Paragraphs.Add("Invalid result - converged to the trivial solution.")
                    Throw New Exception("Invalid result - converged to the trivial solution.")
                End If
            Next

            IObj?.Close()

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
            '_pp.CurrentMaterialStream.Flowsheet.ShowMessage("Inside-Out solver iteration #" & iter_count & ": current objective function (error) value = " & obj_value, IFlowsheet.MessageType.Information)
            Return True
        End Function

    End Class

    <System.Serializable()> Public Class WangHenkeMethod

        Public Shared Function Solve(ByVal nc As Integer, ByVal ns As Integer, ByVal maxits As Integer,
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
                                ByVal IdealK As Boolean, ByVal IdealH As Boolean, ByVal MaxTChange As Double) As Object

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

            Dim ppr As PropertyPackages.RaoultPropertyPackage = Nothing

            If IdealK Or IdealH Then
                ppr = New PropertyPackages.RaoultPropertyPackage
                ppr.Flowsheet = pp.Flowsheet
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
                Case ColumnSpec.SpecType.Component_Fraction,
                ColumnSpec.SpecType.Component_Mass_Flow_Rate,
                ColumnSpec.SpecType.Component_Molar_Flow_Rate,
                ColumnSpec.SpecType.Component_Recovery,
                ColumnSpec.SpecType.Temperature
                    Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCUnsupportedError1"))
            End Select
            Select Case specs("R").SType
                Case ColumnSpec.SpecType.Component_Fraction,
                ColumnSpec.SpecType.Component_Mass_Flow_Rate,
                ColumnSpec.SpecType.Component_Molar_Flow_Rate,
                ColumnSpec.SpecType.Component_Recovery,
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
                    IObj?.SetCurrent
                    If IdealH Then
                        ppr.CurrentMaterialStream.Flowsheet.CheckStatus()
                        Hl(i) = ppr.DW_CalcEnthalpy(x(i), Tj(i), P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(x(i)) / 1000
                        IObj?.SetCurrent
                        Hv(i) = ppr.DW_CalcEnthalpy(y(i), Tj(i), P(i), PropertyPackages.State.Vapor) * ppr.AUX_MMM(y(i)) / 1000
                    Else
                        pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                        Hl(i) = pp.DW_CalcEnthalpy(x(i), Tj(i), P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(x(i)) / 1000
                        IObj?.SetCurrent
                        Hv(i) = pp.DW_CalcEnthalpy(y(i), Tj(i), P(i), PropertyPackages.State.Vapor) * pp.AUX_MMM(y(i)) / 1000
                    End If
                Next
            End If

            IObj?.Paragraphs.Add(String.Format("Vapor Enthalpies: {0}", Hv.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Liquid Enthalpies: {0}", Hl.ToMathArrayString))

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
                        IObj2?.SetCurrent
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
                    If Abs(dTj(i)) > MaxTChange Then Tj(i) = Math.Sign(dTj(i)) * MaxTChange + Tj_ant(i)
                    If Double.IsNaN(Tj(i)) Or Double.IsInfinity(Tj(i)) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))
                Next

                For i = 0 To ns
                    For j = 0 To nc - 1
                        If Double.IsNaN(K(i)(j)) Then K(i)(j) = pp.AUX_PVAPi(j, Tj(i)) / P(i)
                    Next
                    If Double.IsNaN(Tj(i)) Or Double.IsInfinity(Tj(i)) Then Tj(i) = Tj_ant(i)
                Next

                IObj2?.Paragraphs.Add(String.Format("Updated Temperatures: {0}", Tj.ToMathArrayString))

                t_error_ant = t_error
                t_error = 0.0#
                For i = 0 To ns
                    t_error += (Tj(i) - Tj_ant(i)) ^ 2
                Next

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
                        IObj2?.SetCurrent
                        If IdealH Then
                            ppr.CurrentMaterialStream.Flowsheet.CheckStatus()
                            Hl(i) = ppr.DW_CalcEnthalpy(xc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(i)) / 1000
                            IObj2?.SetCurrent
                            Hv(i) = ppr.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(i)) / 1000
                        Else
                            pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                            Hl(i) = pp.DW_CalcEnthalpy(xc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(i)) / 1000
                            IObj2?.SetCurrent
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
                    If ic >= maxits Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCMaxIterationsReached"))
                End If
                If Double.IsNaN(t_error) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))
                If ic = stopatitnumber - 1 Then Exit Do

                pp.CurrentMaterialStream.Flowsheet.CheckStatus()

                Calculator.WriteToConsole("Bubble Point solver T error = " & t_error, 1)

                IObj2?.Close()

            Loop Until t_error < tol(1)

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

            'finished, return arrays

            Return New Object() {Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ic, t_error}

        End Function

    End Class

    <System.Serializable()> Public Class BurninghamOttoMethod

        Public Shared Function Solve(ByVal nc As Integer, ByVal ns As Integer, ByVal maxits As Integer,
                                ByVal tol As Double(), ByVal F As Double(), ByVal V As Double(),
                                ByVal Q As Double(), ByVal L As Double(),
                                ByVal VSS As Double(), ByVal LSS As Double(), ByVal Kval()() As Double,
                                ByVal x()() As Double, ByVal y()() As Double, ByVal z()() As Double,
                                ByVal fc()() As Double,
                                ByVal HF As Double(), ByVal T As Double(), ByVal P As Double(),
                                ByVal stopatitnumber As Integer,
                                ByVal eff() As Double,
                                ByVal pp As PropertyPackages.PropertyPackage,
                                ByVal specs As Dictionary(Of String, SepOps.ColumnSpec),
                                ByVal IdealK As Boolean, ByVal IdealH As Boolean,
                                ByVal MaxTChange As Double,
                                Optional ByVal llextr As Boolean = False) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Solve", "Sum-Rates (SR) Method", "Burningham–Otto Sum-Rates (SR) Method for Absorption and Stripping", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("The species in most absorbers and strippers cover a wide range of volatility. Hence, the BP method of 
                                    solving the MESH equations fails because bubble-point temperature calculations are too sensitive to 
                                    liquid-phase composition, and the stage energy balance is much more sensitive to stage temperatures 
                                    than to interstage flow rates. In this case, Friday and Smith showed that an alternative procedure 
                                    devised by Sujata could be used. This sum-rates (SR) method was further developed in conjunction with 
                                    the tridiagonal-matrix formulation for the modified M equations by Burningham and Otto.")

            IObj?.Paragraphs.Add("Problem specifications consist of conditions and stage locations for feeds, stage pressure, sidestream 
                                    flow rates, stage heat-transfer rates, and number of stages. Tear variables Tj and Vj are assumed to 
                                    initiate the calculations. It is sufficient to assume a set of Vj values based on the assumption of 
                                    constant-molar flows, working up from the absorber bottom using specified vapor feeds and vapor sidestream 
                                    flows, if any. An initial set of Tj values can be obtained from assumed top-stage and bottom-stage values 
                                    and a linear variation with stages in between. Values of xi,j are established by solving the tridiagonal 
                                    matrix by the Thomas algorithm. These values are not normalized but utilized directly to produce new values 
                                    of Lj through the sum-rates equation:")

            IObj?.Paragraphs.Add("<math>L^{(k+1)}_{j}=L^{(k)}_j\sum\limits_{i=1}^{C}{x_{i,j}}</math>")

            IObj?.Paragraphs.Add("where <math_inline>L^{(k)}_j</math_inline> values are obtained from <math_inline>V^{(k)}_j</math_inline> 
                                    values by")

            IObj?.Paragraphs.Add("<math>L_{j}=V_{j+1}+\sum\limits_{m=1}^{j}{(F_{m}-U_{m}-W_{m})-V_{1}}</math>")

            IObj?.Paragraphs.Add("Corresponding values of <math_inline>V^{(k+1)}_j</math_inline> are obtained from a total material balance:")

            IObj?.Paragraphs.Add("<math>V_{j}=L_{j-1}-L_{N}+\sum\limits_{m=j}^{N}{(F_{m}-U_{m}-W_{m})}</math>")

            IObj?.Paragraphs.Add("Normalized <math_inline>x_{i,j}</math_inline> values are calculated and Corresponding values of 
                                    <math_inline>y_{i,j}</math_inline> are computed from")

            IObj?.Paragraphs.Add("<math>y_{i,j}=K_{i,j}x_{i,j}</math>")

            IObj?.Paragraphs.Add("A new set of Tj is obtained by solving the simultaneous energy-balance relations for the N stages. The 
                                    temperatures are embedded in the specific enthalpies for the unspecified vapor and liquid flow rates. 
                                    Typically, these enthalpies are nonlinear in temperature. Therefore, an iterative procedure such as the 
                                    Newton–Raphson method is required.")

            IObj?.Paragraphs.Add("To obtain a new set of Tj from the energy equation, the Newton–Raphson recursion equation is")

            IObj?.Paragraphs.Add("<math>\left(\frac{\partial H_j}{\partial T_{j-1}} \right)^{(r)}\Delta T^{(r)}_{j-1} +\left(\frac{\partial H_j}{\partial T_{j}} \right)^{(r)}\Delta T^{(r)}_{j}+ \left(\frac{\partial H_j}{\partial T_{j+1}} \right)^{(r)}\Delta T^{(r)}_{j+1}=-H^{(r)}_j</math>")

            IObj?.Paragraphs.Add("where")

            IObj?.Paragraphs.Add("<math>\Delta T^{(r)}_{j}=T^{(r+1)}_j-T^{(r)}_j</math>")

            IObj?.Paragraphs.Add("<math>\frac{\partial H_j}{\partial T_{j-1}}=L_{j-1}\frac{\partial h_{L_{j-1}}}{\partial T_{j-1}}</math>")

            IObj?.Paragraphs.Add("<math>\frac{\partial H_j}{\partial T_{j}}=-(L_{j}+U_{j})\frac{\partial h_{L_{j}}}{\partial T_{j}}-(V_j+W_j)\frac{\partial h_{V_{j}}}{\partial T_{j}}</math>")

            IObj?.Paragraphs.Add("<math>\frac{\partial H_j}{\partial T_{j+1}}=V_{j+1}\frac{\partial h_{V_{j+1}}}{\partial T_{j+1}}</math>")

            IObj?.Paragraphs.Add("The partial derivatives are calculated numerically using the values provided by the Property Package.")

            IObj?.Paragraphs.Add("The N relations form a tridiagonal matrix equation that is linear in <math_inline>\Delta T^{(r)}_{j}</math_inline>.")

            IObj?.Paragraphs.Add("The matrix of partial derivatives is called the Jacobian correction matrix. The Thomas algorithm can be employed 
                                    to solve for the set of corrections <math_inline>\Delta T^{(r)}_{j}</math_inline>. New guesses of Tj are then determined from")

            IObj?.Paragraphs.Add("<math>T_j^{(r+1)}=T_j^{(r)}+\Delta T_j^{(r)}</math>")

            IObj?.Paragraphs.Add("When corrections <math_inline>\Delta T^{(r)}_{j}</math_inline> approach zero, the resulting values of Tj are 
                                    used with criteria to determine if convergence has been achieved. If not, before beginning a new k iteration, 
                                    values of Vj and Tj are adjusted. Convergence is rapid for the sum-rates method.")

            Dim ppr As PropertyPackages.RaoultPropertyPackage = Nothing

            If IdealK Or IdealH Then
                ppr = New PropertyPackages.RaoultPropertyPackage
                ppr.Flowsheet = pp.Flowsheet
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
            IObj?.Paragraphs.Add(String.Format("K-values: {0}", K.ToMathArrayString))

            IObj?.Paragraphs.Add("<h2>Calculated Parameters</h2>")

            For i = 0 To ns
                For j = 0 To nc - 1
                    K(i)(j) = Kval(i)(j)
                    IObj?.SetCurrent()
                    If Double.IsNaN(K(i)(j)) Or Double.IsInfinity(K(i)(j)) Or K(i)(j) = 0# Then
                        If llextr Then
                            If i > 0 Then
                                If K(i - 1).Sum > 0.0 Then
                                    K(i) = K(i - 1).Clone
                                End If
                            End If
                        Else
                            K(i)(j) = pp.AUX_PVAPi(j, T(i)) / P(i)
                        End If
                    End If
                Next
            Next

            'step3

            'internal loop
            ic = 0
            Do

                IObj?.SetCurrent()

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj2, "", "Solve", "Sum-Rates (SR) Internal Loop #" & ic, "Burningham–Otto Sum-Rates (SR) Method for Absorption and Stripping", True)

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

                For i = 0 To nc - 1
                    IObj2?.SetCurrent()
                    IObj2?.Paragraphs.Add(String.Format("Calling TDM Solver for Stage #{0}...", i + 1))
                    xt(i) = Tomich.TDMASolve(at(i), bt(i), ct(i), dt(i))
                Next

                IObj2?.SetCurrent()

                IObj2?.Paragraphs.Add(String.Format("TDM solved successfully."))

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
                    Lj(i) = Lj(i) * sumx(i)
                Next

                IObj2?.Paragraphs.Add(String.Format("l: {0}", lc.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("L: {0}", Lj.ToMathArrayString))

                For i = 0 To ns
                    For j = 0 To nc - 1
                        xc(i)(j) = lc(i)(j) / sumx(i)
                        yc(i)(j) = xc(i)(j) * K(i)(j)
                        sumy(i) += yc(i)(j)
                    Next
                Next

                IObj2?.Paragraphs.Add(String.Format("x: {0}", xc.ToMathArrayString))

                For i = 0 To ns
                    For j = 0 To nc - 1
                        yc(i)(j) = yc(i)(j) / sumy(i)
                    Next
                Next

                IObj2?.Paragraphs.Add(String.Format("y: {0}", yc.ToMathArrayString))

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

                IObj2?.Paragraphs.Add(String.Format("v: {0}", vc.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("V: {0}", Vj.ToMathArrayString))

                For i = 0 To ns
                    For j = 0 To nc - 1
                        zc(i)(j) = zc(i)(j) / sumz(i)
                    Next
                Next

                'Dim tmp As Object

                'calculate new temperatures

                IObj2?.Paragraphs.Add("Calculating new temperatures...")

                ''''''''''''''''''''
                Dim H(ns), dHldT(ns), dHvdT(ns), dHdTa(ns), dHdTb(ns), dHdTc(ns), dHl(ns), dHv(ns) As Double

                Dim epsilon As Double = 0.0001

                If doparallel Then

                    Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, ns + 1, poptions,
                                                             Sub(ipar)
                                                                 If IdealH Then
                                                                     Hl(ipar) = ppr.DW_CalcEnthalpy(xc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(ipar)) / 1000
                                                                     dHl(ipar) = ppr.DW_CalcEnthalpy(xc(ipar), Tj(ipar) - epsilon, P(ipar), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(ipar)) / 1000
                                                                     If llextr Then
                                                                         Hv(ipar) = ppr.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * ppr.AUX_MMM(yc(ipar)) / 1000
                                                                         dHv(ipar) = ppr.DW_CalcEnthalpy(yc(ipar), Tj(ipar) - epsilon, P(ipar), PropertyPackages.State.Liquid) * ppr.AUX_MMM(yc(ipar)) / 1000
                                                                     Else
                                                                         Hv(ipar) = ppr.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(ipar)) / 1000
                                                                         dHv(ipar) = ppr.DW_CalcEnthalpy(yc(ipar), Tj(ipar) - epsilon, P(ipar), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(ipar)) / 1000
                                                                     End If
                                                                 Else
                                                                     Hl(ipar) = pp.DW_CalcEnthalpy(xc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(ipar)) / 1000
                                                                     dHl(ipar) = pp.DW_CalcEnthalpy(xc(ipar), Tj(ipar) - epsilon, P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(ipar)) / 1000
                                                                     If llextr Then
                                                                         Hv(ipar) = pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(yc(ipar)) / 1000
                                                                         dHv(ipar) = pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar) - epsilon, P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(yc(ipar)) / 1000
                                                                     Else
                                                                         Hv(ipar) = pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(ipar)) / 1000
                                                                         dHv(ipar) = pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar) - epsilon, P(ipar), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(ipar)) / 1000
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
                            IObj2?.SetCurrent()
                            Hl(i) = ppr.DW_CalcEnthalpy(xc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(i)) / 1000
                            IObj2?.SetCurrent()
                            dHl(i) = ppr.DW_CalcEnthalpy(xc(i), Tj(i) - epsilon, P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(i)) / 1000
                            IObj2?.SetCurrent()
                            If llextr Then
                                Hv(i) = ppr.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(yc(i)) / 1000
                                IObj2?.SetCurrent()
                                dHv(i) = ppr.DW_CalcEnthalpy(yc(i), Tj(i) - epsilon, P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(yc(i)) / 1000
                            Else
                                Hv(i) = ppr.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(i)) / 1000
                                IObj2?.SetCurrent()
                                dHv(i) = ppr.DW_CalcEnthalpy(yc(i), Tj(i) - epsilon, P(i), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(i)) / 1000
                            End If
                            ppr.CurrentMaterialStream.Flowsheet.CheckStatus()
                        Next
                    Else
                        For i = 0 To ns
                            IObj2?.SetCurrent()
                            Hl(i) = pp.DW_CalcEnthalpy(xc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(i)) / 1000
                            IObj2?.SetCurrent()
                            dHl(i) = pp.DW_CalcEnthalpy(xc(i), Tj(i) - epsilon, P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(i)) / 1000
                            IObj2?.SetCurrent()
                            If llextr Then
                                Hv(i) = pp.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(yc(i)) / 1000
                                IObj2?.SetCurrent()
                                dHv(i) = pp.DW_CalcEnthalpy(yc(i), Tj(i) - epsilon, P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(yc(i)) / 1000
                            Else
                                Hv(i) = pp.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(i)) / 1000
                                IObj2?.SetCurrent()
                                dHv(i) = pp.DW_CalcEnthalpy(yc(i), Tj(i) - epsilon, P(i), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(i)) / 1000
                            End If
                            pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                        Next
                    End If
                End If

                IObj2?.Paragraphs.Add(String.Format("HL: {0}", Hl.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("HV: {0}", Hl.ToMathArrayString))

                For i = 0 To ns
                    If i = 0 Then
                        H(i) = Vj(i + 1) * Hv(i + 1) + Fj(i) * Hfj(i) - (Lj(i) + LSSj(i)) * Hl(i) - (Vj(i) + VSSj(i)) * Hv(i) - Q(i)
                    ElseIf i = ns Then
                        H(i) = Lj(i - 1) * Hl(i - 1) + Fj(i) * Hfj(i) - (Lj(i) + LSSj(i)) * Hl(i) - (Vj(i) + VSSj(i)) * Hv(i) - Q(i)
                    Else
                        H(i) = Lj(i - 1) * Hl(i - 1) + Vj(i + 1) * Hv(i + 1) + Fj(i) * Hfj(i) - (Lj(i) + LSSj(i)) * Hl(i) - (Vj(i) + VSSj(i)) * Hv(i) - Q(i)
                    End If
                    dHldT(i) = (Hl(i) - dHl(i)) / epsilon
                    dHvdT(i) = (Hv(i) - dHv(i)) / epsilon
                Next

                IObj2?.Paragraphs.Add(("<mi>\frac{\partial h_L}{\partial T}</mi>: " & dHldT.ToMathArrayString))
                IObj2?.Paragraphs.Add(("<mi>\frac{\partial h_V}{\partial T}</mi>: " & dHvdT.ToMathArrayString))

                For i = 0 To ns
                    If i > 0 Then dHdTa(i) = Lj(i - 1) * dHldT(i - 1)
                    dHdTb(i) = -(Lj(i) + LSSj(i)) * dHldT(i) - (Vj(i) + VSSj(i)) * dHvdT(i)
                    If i < ns Then dHdTc(i) = Vj(i + 1) * dHvdT(i + 1)
                Next

                IObj2?.Paragraphs.Add(String.Format("H: {0}", H.ToMathArrayString))

                Dim ath(ns), bth(ns), cth(ns), dth(ns), xth(ns) As Double

                For i = 0 To ns
                    dth(i) = -H(i)
                    bth(i) = dHdTb(i)
                    If i < ns Then cth(i) = dHdTc(i)
                    If i > 0 Then ath(i) = dHdTa(i)
                Next

                'solve matrices
                'tomich

                IObj2?.Paragraphs.Add("Calling TDM Solver to solve for enthalpies/temperatures")

                IObj2?.SetCurrent()

                xth = Tomich.TDMASolve(ath, bth, cth, dth)

                Dim tmp As Object

                IObj2?.Paragraphs.Add(String.Format("Calculated Temperature perturbations: {0}", xth.ToMathArrayString))

                Dim deltat As Double()
                Dim maxdt As Double = xth.Select(Function(tp) Abs(tp)).Max

                If maxdt > MaxTChange Then
                    deltat = xth.Select(Function(tp) tp / maxdt * MaxTChange).ToArray()
                Else
                    deltat = xth
                End If

                t_error = 0.0#
                comperror = 0.0#
                For i = 0 To ns
                    Tj_ant(i) = Tj(i)
                    Tj(i) = Tj(i) + deltat(i)
                    If Double.IsNaN(Tj(i)) Or Double.IsInfinity(Tj(i)) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))
                    If IdealK Then
                        IObj2?.SetCurrent()
                        If llextr Then
                            tmp = ppr.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i), "LL")
                        Else
                            tmp = ppr.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i))
                        End If
                    Else
                        IObj2?.SetCurrent()
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
                                IObj2?.SetCurrent()
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

                IObj2?.Paragraphs.Add(String.Format("Updated Temperatures: {0}", Tj.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("Updated K-values: {0}", K.ToMathArrayString))

                For i = 0 To ns
                    For j = 0 To nc - 1
                        yc(i)(j) = yc(i)(j) / sumy(i)
                    Next
                Next

                IObj2?.Paragraphs.Add(String.Format("Updated y: {0}", yc.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("Temperature error: {0}", t_error))

                IObj2?.Paragraphs.Add(String.Format("Composition error: {0}", comperror))

                IObj2?.Paragraphs.Add(String.Format("Combined Temperature/Composition error: {0}", t_error + comperror))

                ic = ic + 1

                If Not IdealH And Not IdealK Then
                    If ic >= maxits Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCMaxIterationsReached"))
                End If

                If Double.IsNaN(t_error) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))
                If Double.IsNaN(comperror) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))

                pp.CurrentMaterialStream.Flowsheet.CheckStatus()

                Calculator.WriteToConsole("Sum Rates solver T error = " & t_error, 1)
                Calculator.WriteToConsole("Sum Rates solver composition error = " & comperror, 1)

                IObj2?.Close()

                pp.CurrentMaterialStream.Flowsheet.ShowMessage("Sum-Rates solver: iteration #" & ic.ToString & ", Temperature error = " & t_error.ToString, IFlowsheet.MessageType.Information)
                pp.CurrentMaterialStream.Flowsheet.ShowMessage("Sum-Rates solver: iteration #" & ic.ToString & ", Composition error = " & comperror.ToString, IFlowsheet.MessageType.Information)
                pp.CurrentMaterialStream.Flowsheet.ShowMessage("Sum-Rates solver: iteration #" & ic.ToString & ", combined Temperature/Composition error = " & (t_error + comperror).ToString, IFlowsheet.MessageType.Information)

            Loop Until t_error <= tol(1) And comperror <= tol(1)

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

            For Each Ki In K
                If pp.AUX_CheckTrivial(Ki) Then
                    IObj?.Paragraphs.Add("Invalid result - converged to the trivial solution.")
                    Throw New Exception("Invalid result - converged to the trivial solution.")
                End If
            Next

            ' finished, return arrays

            Return New Object() {Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ic, t_error}

        End Function

    End Class

    <System.Serializable()> Public Class NaphtaliSandholmMethod

        Dim _IObj As Inspector.InspectorItem

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
        Dim _maxT, _maxvc, _maxlc, _maxtchange, _Tj_ant() As Double

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

            _IObj?.SetCurrent

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "FunctionValue", "Simultaneous Correction (SC) Method MEH Equations Calculator", "Naphtali-Sandholm Simultaneous Correction (SC) Method for Distillation, Absorption and Stripping", True)

            IObj?.SetCurrent()

            Dim x As Double() = xl.ExpY

            IObj?.Paragraphs.Add(String.Format("Input Variables: {0}", xl.ToMathArrayString))

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
            Dim Tj(ns), Tj_ant(ns), vc(ns)(), lc(ns)(), zc(ns)(), Vj(ns), Lj(ns), xc(ns)(), yc(ns)(), Kval(ns)() As Double

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
                If Abs(Tj(i) - _Tj_ant(i)) > _maxtchange Then
                    Tj(i) = _Tj_ant(i) + Sign(Tj(i) - _Tj_ant(i)) * _maxtchange
                End If
                If Double.IsNaN(Tj(i)) Or Double.IsInfinity(Tj(i)) Then Throw New Exception(_pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))
                For j = 0 To nc - 1
                    vc(i)(j) = x(i * (2 * nc + 1) + j + 1) * _maxvc
                    lc(i)(j) = x(i * (2 * nc + 1) + j + 1 + nc) * _maxlc
                Next
            Next

            _Tj_ant = Tj.Clone

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
                        IObj?.SetCurrent
                        If llextr Then
                            tmp0 = _ppr.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i), "LL")
                        Else
                            tmp0 = _ppr.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i))
                        End If
                    Else
                        IObj?.SetCurrent
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
                            IObj?.SetCurrent
                            If llextr Then
                                Hv(i) = _ppr.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * _ppr.AUX_MMM(yc(i)) / 1000
                            Else
                                Hv(i) = _ppr.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Vapor) * _ppr.AUX_MMM(yc(i)) / 1000
                            End If
                        Else
                            IObj?.SetCurrent
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
                        IObj?.SetCurrent
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


            IObj?.Paragraphs.Add(String.Format("M Equation Deviations: {0}", M.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("E Equation Deviations: {0}", E.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("H Equation Deviations: {0}", H.ToMathArrayString))

            If Not grad Then
                _pp.CurrentMaterialStream.Flowsheet.ShowMessage("NS solver: current objective function (error) value = " & errors.AbsSqrSumY, IFlowsheet.MessageType.Information)
                _pp.CurrentMaterialStream.Flowsheet.CheckStatus()
            End If

            For i = 0 To errors.Length - 1
                If Double.IsNaN(errors(i)) Or Double.IsInfinity(errors(i)) Then errors(i) = 10000000000.0
            Next

            IObj?.Paragraphs.Add(String.Format("Total Error: {0}", errors.AbsSqrSumY))

            IObj?.Close()

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
                                ByVal epsilon As Double,
                                ByVal Solver As OptimizationMethod,
                                ByVal LowerBound As Double, ByVal UpperBound As Double,
                                ByVal SimplexPreconditioning As Boolean,
                                ByVal IdealK As Boolean, ByVal IdealH As Boolean,
                                ByVal MaxTChange As Double,
                                Optional ByVal LLEX As Boolean = False) As Object

            _Tj_ant = T.Clone

            _maxtchange = MaxTChange

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

            ik = IdealK
            ih = IdealH

            If ik Or ih Then
                _ppr = New PropertyPackages.RaoultPropertyPackage
                _ppr.Flowsheet = pp.Flowsheet
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

            IObj?.Paragraphs.Add("Creating variable vectors...")

            IObj?.Paragraphs.Add(String.Format("Initial Variable Values: {0}", xvar.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Lower Bounds: {0}", lb.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Upper Bounds: {0}", ub.ToMathArrayString))

            _IObj = IObj

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

            IObj?.Paragraphs.Add(String.Format("Final Variable Values: {0}", xvar.ToMathArrayString))

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
            Dim K(ns, nc - 1) As Double
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
