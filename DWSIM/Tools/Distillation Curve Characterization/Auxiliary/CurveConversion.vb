'    Petroleum Distillation Curve Conversion Routines 
'    Copyright 2009 Daniel Wagner O. de Medeiros
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

Namespace DWSIM.Utilities.PetroleumCharacterization.Methods

    Public Class DistillationCurveConversion

        ''' <summary>
        ''' Converts ASTM D86 points to ASTM D2892 (TBP).
        ''' </summary>
        ''' <param name="Tastmd86">Temperatures of the D86 distillation curve, in K.</param>
        ''' <returns>Temperatures of the TBP distillation curve, in K.</returns>
        ''' <remarks></remarks>
        Public Shared Function ASTMD86ToPEV_Riazi(ByVal Tastmd86() As Double) As Double()

            Dim a(6), b(6), Tpev(6) As Double
            Dim i As Integer

            a(0) = 0.9177
            a(1) = 0.5564
            a(2) = 0.7617
            a(3) = 0.9013
            a(4) = 0.8821
            a(5) = 0.9552
            a(6) = 0.8177

            b(0) = 1.0019
            b(1) = 1.09
            b(2) = 1.0425
            b(3) = 1.0176
            b(4) = 1.0226
            b(5) = 1.011
            b(6) = 1.0355

            For i = 0 To UBound(Tastmd86)
                Tpev(i) = a(i) * Tastmd86(i) ^ b(i)
            Next

            Return Tpev

        End Function

        ''' <summary>
        ''' Converts ASTM D2887 points to ASTM D2892 (TBP).
        ''' </summary>
        ''' <param name="Tastmd2887">Temperatures of the D2887 distillation curve, in K.</param>
        ''' <returns>Temperatures of the TBP distillation curve, in K.</returns>
        ''' <remarks></remarks>
        Public Shared Function ASTMD2887ToPEV_Daubert(ByVal Tastmd2887() As Double) As Double()

            Dim a(6), b(6), Tpev(7), dT(6) As Double
            Dim i, n As Integer

            n = UBound(Tastmd2887)

            For i = 0 To n
                Tastmd2887(i) = (Tastmd2887(i) - 273.15) * 9 / 5 + 32
            Next

            a(0) = 0.15779
            a(1) = 0.011903
            a(2) = 0.05342
            a(3) = 0.19861
            a(4) = 0.31531
            a(5) = 0.97476
            a(6) = 0.02172

            b(0) = 1.4296
            b(1) = 2.0253
            b(2) = 1.6988
            b(3) = 1.3975
            b(4) = 1.2938
            b(5) = 0.8723
            b(6) = 1.9733

            For i = 0 To n - 1
                dT(i) = a(i) * (Tastmd2887(i + 1) - Tastmd2887(i)) ^ b(i)
            Next

            Tpev(0) = Tastmd2887(3) - dT(0) - dT(1) - dT(2)   'T5
            Tpev(1) = Tastmd2887(3) - dT(0) - dT(1)  'T10
            Tpev(2) = Tastmd2887(3) - dT(0)  'T30
            Tpev(3) = Tastmd2887(3)  'T50
            Tpev(4) = Tastmd2887(3) + dT(3) 'T70
            Tpev(5) = Tastmd2887(3) + dT(3) + dT(4) 'T90
            Tpev(6) = Tastmd2887(3) + dT(3) + dT(4) + dT(5) 'T95
            Tpev(7) = Tastmd2887(3) + dT(3) + dT(4) + dT(5) + dT(6) 'T100

            For i = 0 To n
                Tpev(i) = (Tpev(i) - 32) / 9 * 5 + 273.15
            Next

            Return Tpev

        End Function

        ''' <summary>
        ''' Converts sub-atmospheric TBP to atmospheric TBP curve.
        ''' </summary>
        ''' <param name="T">Temperature in K.</param>
        ''' <param name="P">Pressure in Pa.</param>
        ''' <param name="Kw">Watson K</param>
        ''' <returns>TBP curve temperatures in K.</returns>
        ''' <remarks></remarks>
        Public Shared Function PEVsubToPEV_MaxwellBonnel(ByVal T As Double, ByVal P As Double, ByVal Kw As Double) As Double

            P = P * 760 / 101325
            T = 1.8 * T

            Dim X, Tb_, Tb_ant As Double
            Dim f As Double
            Dim i As Integer

            i = 0
            Do
                Tb_ant = Tb_

                X = (-5.994296 + 0.972546 * Log10(P)) / (95.76 * Log10(P) - 2663.129)
                Tb_ = -748.1 * X / (0.0002867 - 0.2145 * X - 1 / T)

                If (Tb_ - 459.7) < 200 Then
                    f = 0
                ElseIf (Tb_ - 459.7) > 400 Then
                    f = 1
                Else
                    f = (Tb_ - 659.7) / 200
                End If

                Tb_ = Tb_ - 2.5 * f * (Kw - 12) * Log10(P / 760)

                i += 1
            Loop Until Abs(Tb_ - Tb_ant) < 0.001 Or i > 1000

            Return Tb_ / 1.8

        End Function

        ''' <summary>
        ''' Converts ASTM D86 points to ASTM D2892 (TBP).
        ''' </summary>
        ''' <param name="Tastm">Temperatures of the D1160 distillation curve, in K.</param>
        ''' <returns>Sub-atm TBP curve temperatures in K.</returns>
        ''' <remarks></remarks>
        Public Shared Function ASTMD1160ToPEVsub_Wauquier(ByVal Tastm() As Double) As Double()

            Dim j, fracv, i1, i2, i As Integer
            Dim f(0 To 1, 0 To 10) As Double
            Dim Tpev10(6), fv(6) As Double

            fv(0) = 0
            fv(1) = 10
            fv(2) = 30
            fv(3) = 50
            fv(4) = 70
            fv(5) = 90
            fv(6) = 100

            f(0, 0) = 0
            f(0, 1) = 20
            f(0, 2) = 35.5
            f(0, 3) = 47.5
            f(0, 4) = 57
            f(0, 5) = 64
            f(0, 6) = 70
            f(0, 7) = 75
            f(0, 8) = 82.5
            f(0, 9) = 91
            f(0, 10) = 100
            f(1, 0) = 0
            f(1, 1) = 13
            f(1, 2) = 24
            f(1, 3) = 34.5
            f(1, 4) = 44
            f(1, 5) = 53.5
            f(1, 6) = 63
            f(1, 7) = 72
            f(1, 8) = 81.5
            f(1, 9) = 90.5
            f(1, 10) = 100

            For i = 0 To 6
                fracv = Convert.ToInt32(fv(i) / 20 + 0.5)
                If fracv >= 3 Then
                    Tpev10(i) = Tastm(i)
                Else
                    Tpev10(i) = Tastm(3)
                    i1 = 0
                    For j = 3 To fracv + 1 Step -1
                        If j = 2 Then i1 = 1
                        i2 = Int(Abs(Tastm(j) - Tastm(j - 1)) / 10) - 1
                        Tpev10(i) = Tpev10(i) - (Abs(Tastm(j) - Tastm(j - 1)) / 10 - i2) * (f(i1, i2 + 1) - f(i1, i2)) - f(i1, i2)
                    Next j
                End If
            Next

            Return Tpev10

        End Function

        Class TBPFit

            Private _x, _y As Double()
            Private sum As Double
            Private its As Integer = 0

            Public Function GetCoeffs(ByVal x As Double(), ByVal y As Double(), ByVal inest() As Double, ByVal epsg As Double, ByVal epsf As Double, ByVal epsx As Double, ByVal maxits As Integer) As Object

                Dim lmsolve As New MathEx.LM.levenbergmarquardt
                lmsolve.DefineFuncGradDelegate(AddressOf fv)

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

            Public Sub fv(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

                If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
                If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

                'A + B*FV + C*FV^2 + D*FV^3 + E*FV^4 + F*FV^5 + G*FV^6
                sum = 0
                Dim i As Integer
                If iflag = 1 Then
                    i = 1
                    Do
                        fvec(i) = -_y(i - 1) + (x(1) + x(2) * _x(i - 1) + x(3) * _x(i - 1) ^ 2 + x(4) * _x(i - 1) ^ 3 + x(5) * _x(i - 1) ^ 4 + x(6) * _x(i - 1) ^ 5 + x(7) * _x(i - 1) ^ 6)
                        sum += (fvec(i)) ^ 2
                        i = i + 1
                    Loop Until i = UBound(_y) + 2
                ElseIf iflag = 2 Then
                    i = 1
                    Do
                        'A + B*FV + C*FV^2 + D*FV^3 + E*FV^4 + F*FV^5 + G*FV^6
                        fjac(i, 1) = 0
                        fjac(i, 2) = _x(i - 1)
                        fjac(i, 3) = _x(i - 1) ^ 2
                        fjac(i, 4) = _x(i - 1) ^ 3
                        fjac(i, 5) = _x(i - 1) ^ 4
                        fjac(i, 6) = _x(i - 1) ^ 5
                        fjac(i, 7) = _x(i - 1) ^ 6
                        i = i + 1
                    Loop Until i = UBound(_y) + 2
                End If

                its += 1

            End Sub

        End Class

    End Class

End Namespace
