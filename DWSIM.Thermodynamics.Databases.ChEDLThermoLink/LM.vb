'    Levenberg-Marquadt Calculation Routines 
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

Imports System.Math
Imports DWSIM.MathOps

Public Class LMFit



    Public Enum FitType
        Pvap = 0
        Cp = 1
        LiqVisc = 2
        HVap = 3
        LiqDens = 4
        SecondDegreePoly = 5
    End Enum

    Private _x, _y As Double()
    Private sum As Double
    Private its As Integer = 0

    Public Function GetCoeffs(ByVal x As Double(), ByVal y As Double(), ByVal inest As Double(), ByVal fittype As FitType,
                            ByVal epsg As Double, ByVal epsf As Double, ByVal epsx As Double, ByVal maxits As Integer) As Object

        Dim lmsolve As New MathEx.LM.levenbergmarquardt
        Select Case fittype
            Case LMFit.FitType.Pvap
                lmsolve.DefineFuncGradDelegate(AddressOf fvpvap)
            Case LMFit.FitType.Cp
                lmsolve.DefineFuncGradDelegate(AddressOf fvcp)
            Case LMFit.FitType.LiqVisc
                lmsolve.DefineFuncGradDelegate(AddressOf fvlvisc)
            Case LMFit.FitType.HVap
                lmsolve.DefineFuncGradDelegate(AddressOf fvhvap)
            Case LMFit.FitType.LiqDens
                lmsolve.DefineFuncGradDelegate(AddressOf fvliqdens)
            Case LMFit.FitType.SecondDegreePoly
                lmsolve.DefineFuncGradDelegate(AddressOf fvsdp)
        End Select

        Dim newc(UBound(inest) + 1) As Double
        Dim i As Integer = 1
        Do
            newc(i) = inest(i - 1)
            i = i + 1
        Loop Until i = UBound(inest) + 2

        Me._x = x
        Me._y = y

        Dim info As Integer = 56

        its = 0
        lmsolve.levenbergmarquardtminimize(inest.Length, _x.Length, newc, epsg, epsf, epsx, maxits, info)

        Dim coeffs(UBound(inest)) As Double

        i = 0
        Do
            coeffs(i) = newc(i + 1)
            i = i + 1
        Loop Until i = UBound(inest) + 1

        Return New Object() {coeffs, info, sum, its}

    End Function

    Public Sub fvpvap(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

        If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
        If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

        sum = 0.0#
        Dim i As Integer
        If iflag = 1 Then
            i = 1
            Do
                fvec(i) = -_y(i - 1) + (Math.Exp(x(1) + x(2) / _x(i - 1) + x(3) * Math.Log(_x(i - 1)) + x(4) * _x(i - 1) ^ x(5)))
                sum += (fvec(i)) ^ 2
                i = i + 1
            Loop Until i = UBound(_y) + 2
        ElseIf iflag = 2 Then
            Dim fval As Double = 0
            i = 1
            Do
                'Math.Exp(A + B / T + C * Math.Log(T) + D * T ^ E)
                fval = (Math.Exp(x(1) + x(2) / _x(i - 1) + x(3) * Math.Log(_x(i - 1)) + x(4) * _x(i - 1) ^ x(5)))
                fjac(i, 1) = fval
                fjac(i, 2) = fval * 1 / _x(i - 1)
                fjac(i, 3) = fval * Math.Log(_x(i - 1))
                fjac(i, 4) = fval * _x(i - 1) ^ x(5)
                fjac(i, 5) = fval * x(5) * _x(i - 1) ^ x(5) * Math.Log(_x(i - 1))
                i = i + 1
            Loop Until i = UBound(_y) + 2
        End If

        its += 1

    End Sub

    Public Sub fvcp(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

        If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
        If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

        'A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4
        sum = 0.0#
        Dim i As Integer
        If iflag = 1 Then
            i = 1
            Do
                fvec(i) = -_y(i - 1) + (x(1) + x(2) * _x(i - 1) + x(3) * _x(i - 1) ^ 2 + x(4) * _x(i - 1) ^ 3 + x(5) * _x(i - 1) ^ 4)
                sum += (fvec(i)) ^ 2
                i = i + 1
            Loop Until i = UBound(_y) + 2
        ElseIf iflag = 2 Then
            i = 1
            Do
                'A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4
                fjac(i, 1) = 1
                fjac(i, 2) = _x(i - 1)
                fjac(i, 3) = _x(i - 1) ^ 2
                fjac(i, 4) = _x(i - 1) ^ 3
                fjac(i, 5) = _x(i - 1) ^ 4
                i = i + 1
            Loop Until i = UBound(_y) + 2
        End If

        its += 1

    End Sub

    Public Sub fvlvisc(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

        If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
        If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

        sum = 0
        Dim i As Integer
        If iflag = 1 Then
            i = 1
            Do
                fvec(i) = -_y(i - 1) + (Math.Exp(x(1) + x(2) / _x(i - 1) + x(3) * Math.Log(_x(i - 1)) + x(4) * _x(i - 1) ^ x(5)))
                sum += (fvec(i)) ^ 2
                i = i + 1
            Loop Until i = UBound(_y) + 2
        ElseIf iflag = 2 Then
            Dim fval As Double = 0
            i = 1
            Do
                'Math.Exp(A + B / T + C * Math.Log(T) + D * T ^ E)
                fval = (Math.Exp(x(1) + x(2) / _x(i - 1) + x(3) * Math.Log(_x(i - 1)) + x(4) * _x(i - 1) ^ x(5)))
                fjac(i, 1) = fval
                fjac(i, 2) = fval * 1 / _x(i - 1)
                fjac(i, 3) = fval * Math.Log(_x(i - 1))
                fjac(i, 4) = fval * _x(i - 1) ^ x(5)
                fjac(i, 5) = fval * x(5) * _x(i - 1) ^ x(5) * Math.Log(_x(i - 1))
                i = i + 1
            Loop Until i = UBound(_y) + 2
        End If

        its += 1

    End Sub

    Public Sub fvhvap(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

        If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
        If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

        'A * (1 - Tr) ^ (B + C * Tr + D * Tr ^ 2)
        sum = 0.0#
        Dim i As Integer
        If iflag = 1 Then
            i = 1
            Do
                fvec(i) = -_y(i - 1) + (x(1) * (1 - _x(i - 1)) ^ (x(2) + x(3) * _x(i - 1) + x(4) * _x(i - 1) ^ 2))
                sum += (fvec(i)) ^ 2
                i = i + 1
            Loop Until i = UBound(_y) + 2
        ElseIf iflag = 2 Then
            i = 1
            Do
                Dim fval As Double = 0
                'A * (1 - Tr) ^ (B + C * Tr + D * Tr ^ 2)
                fval = (x(1) * (1 - _x(i - 1)) ^ (x(2) + x(3) * _x(i - 1) + x(4) * _x(i - 1) ^ 2))
                fjac(i, 1) = fval
                fjac(i, 2) = fval
                fjac(i, 3) = fval * _x(i - 1)
                fjac(i, 4) = fval * _x(i - 1) ^ 2
                i = i + 1
            Loop Until i = UBound(_y) + 2
        End If

        its += 1

    End Sub

    Public Sub fvliqdens(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

        If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
        If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

        'a / b^[1 + (1 - t/c)^d]
        sum = 0.0#
        Dim i As Integer
        If iflag = 1 Then
            i = 1
            Do
                fvec(i) = -_y(i - 1) + (x(1) / x(2) ^ (1 + (1 - _x(i - 1) / x(3)) ^ x(4)))
                sum += (fvec(i)) ^ 2
                i = i + 1
            Loop Until i = UBound(_y) + 2
        ElseIf iflag = 2 Then
            i = 1
            Do
                'a / b^[1 + (1 - t/c)^d]
                fjac(i, 1) = 1 / x(2) ^ (1 + (1 - _x(i - 1) / x(3)) ^ x(4))
                fjac(i, 2) = -(x(1) * (x(3) - _x(i - 1)) ^ x(4) + x(1) * x(3) ^ x(4)) / (x(2) ^ (((x(3) - _x(i - 1)) ^ x(4) + 2 * x(3) ^ x(4)) / x(3) ^ x(4)) * x(3) ^ x(4))
                fjac(i, 3) = x(1) * Log(x(2)) * x(4) * (x(3) - _x(i - 1)) ^ x(4) * _x(i - 1) / (x(2) ^ (((x(3) - _x(i - 1)) ^ x(4) + x(3) ^ x(4)) / x(3) ^ x(4)) * x(3) ^ (x(4) + 1) * _x(i - 1) - x(2) ^ (((x(3) - _x(i - 1)) ^ x(4) + x(3) ^ x(4)) / x(3) ^ x(4)) * x(3) ^ (x(4) + 2))
                fjac(i, 4) = -(x(1) * Log(x(2)) * Log(x(3) - _x(i - 1)) - x(1) * Log(x(2)) * Log(x(3))) * (x(3) - _x(i - 1)) ^ x(4) / (x(2) ^ (((x(3) - _x(i - 1)) ^ x(4) + x(3) ^ x(4)) / x(3) ^ x(4)) * x(3) ^ x(4))
                fjac(i, 5) = 0
                i = i + 1
            Loop Until i = UBound(_y) + 2
        End If

        its += 1

    End Sub

    Public Sub fvsdp(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

        If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
        If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

        'A + B * T + C * T ^ 2
        sum = 0.0#
        Dim i As Integer
        If iflag = 1 Then
            i = 1
            Do
                fvec(i) = -_y(i - 1) + (x(1) + x(2) * _x(i - 1) + x(3) * _x(i - 1) ^ 2)
                sum += (fvec(i)) ^ 2
                i = i + 1
            Loop Until i = UBound(_y) + 2
        ElseIf iflag = 2 Then
            i = 1
            Do
                'A + B * T + C * T ^ 2
                fjac(i, 1) = 1
                fjac(i, 2) = _x(i - 1)
                fjac(i, 3) = _x(i - 1) ^ 2
                i = i + 1
            Loop Until i = UBound(_y) + 2
        End If

        its += 1

    End Sub

End Class


