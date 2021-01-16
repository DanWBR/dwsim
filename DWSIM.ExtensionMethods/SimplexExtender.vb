'    DotNumerics Simplex Extender
'    Copyright 2015 Gregor Reichert, Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU Lesser General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports DotNumerics.Optimization

Public Module SimplexExtender

    Public Delegate Function ObjectiveFunction(ByVal x() As Double) As Double

    ''' <summary>
    ''' Simplified implementation of Nelder-Mead-Simplex-Downhill algorithm. No "Reduction" and no "Expansion" implemented yet.
    ''' https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method
    ''' </summary>
    ''' <param name="simplexsolver">simplex solver instance</param>
    ''' <param name="objfunc">objective function delegate</param>
    ''' <param name="Var">optimization variables</param>
    ''' <returns>the values of the variables that minimize the objective function value</returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function ComputeMin2(simplexsolver As Simplex, objfunc As ObjectiveFunction, ByVal Var() As OptBoundVariable) As Double()

        Dim cnt As Integer = 0
        Dim i, k, IdxMax As Integer
        Dim Dx, FVnew, LF, LF1 As Double
        Dim Beta As Double = 0.5
        'Dimension 0: point number
        'Dimension 1: coordinates of points; last column = 1 is indicating "at limiting border"
        Dim Points(Var.Length, Var.Length - 1) As Double
        Dim FuncVal(Var.Length) As Double
        Dim Pt(Var.Length - 1) As Double
        Dim CenterPt(Var.Length - 1) As Double

        'Calculate initial value
        For i = 0 To Var.Length - 1
            'Pt(i) = (Var(i).UpperBound - Var(i).LowerBound) / 2
            'Pt(i) = 0
            Pt(i) = Var(i).InitialGuess
        Next

        'Initialise points
        For k = 0 To Var.Length  'points
            For i = 0 To Var.Length - 1 'coordinates
                Points(k, i) = Pt(i)
            Next
        Next
        For k = 1 To Var.Length
            Dx = (Var(k - 1).UpperBound - Var(k - 1).LowerBound) / 10
            Points(k, k - 1) += Dx
        Next

        'Calculate point values
        For k = 0 To Var.Length
            For i = 0 To Var.Length - 1
                Pt(i) = Points(k, i)
            Next
            FuncVal(k) = objfunc(Pt)
        Next

        Do
            cnt += 1

            'Search worst value e.g. maximum Gibbs Enthalpy
            IdxMax = 0
            For k = 1 To Var.Length
                If FuncVal(k) > FuncVal(IdxMax) Then IdxMax = k
            Next

            'Calculate center point as average from all points except max
            For i = 0 To Var.Length - 1
                CenterPt(i) = 0
            Next
            For k = 0 To Var.Length
                If k <> IdxMax Then
                    For i = 0 To Var.Length - 1
                        CenterPt(i) += Points(k, i) / Var.Length
                    Next
                End If
            Next

            'reflect worst point at center point
            LF = 1
            For i = 0 To Var.Length - 1
                LF1 = 1
                Pt(i) = CenterPt(i) + CenterPt(i) - Points(IdxMax, i)
                'Check if point is inside bounds
                If Pt(i) < Var(i).LowerBound Then
                    LF1 = (Var(i).LowerBound - CenterPt(i)) / (Points(IdxMax, i) - CenterPt(i))
                End If
                If LF1 < LF Then LF = LF1
                If Pt(i) > Var(i).UpperBound Then
                    LF1 = (Var(i).UpperBound - CenterPt(i)) / (Points(IdxMax, i) - CenterPt(i))
                End If
                If LF1 < LF Then LF = -LF1
            Next
            If LF < 1 Then
                For i = 0 To Var.Length - 1
                    Pt(i) = CenterPt(i) + LF * (CenterPt(i) - Points(IdxMax, i))
                Next
            End If
            FVnew = objfunc(Pt)

            If FVnew < FuncVal(IdxMax) Then
                'replace worst point by new one
                FuncVal(IdxMax) = FVnew
                For i = 0 To Var.Length - 1
                    Points(IdxMax, i) = Pt(i)
                Next
            Else
                'contract worst point to center point
                For i = 0 To Var.Length - 1
                    Pt(i) = 0.5 * CenterPt(i) + 0.5 * Points(IdxMax, i)
                Next
                FVnew = objfunc(Pt)

                'check if solution is not improving anymore
                If FVnew > FuncVal(IdxMax) - simplexsolver.Tolerance Or cnt > simplexsolver.MaxFunEvaluations Then Exit Do

                'and replace worst value by contracted value
                FuncVal(IdxMax) = FVnew
                For i = 0 To Var.Length - 1
                    Points(IdxMax, i) = Pt(i)
                Next
            End If
        Loop

        'Search best value to be returned e.g. minimum Gibbs Enthalpy
        IdxMax = 0
        For k = 1 To Var.Length
            If FuncVal(k) < FuncVal(IdxMax) Then IdxMax = k
        Next
        For i = 0 To Var.Length - 1
            Pt(i) = Points(IdxMax, i)
        Next
        Return Pt
    End Function


End Module

