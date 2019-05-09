Namespace MathEx

    Public Class LMFit

        Public Enum FitType
            SecondDegreePoly = 0
            ThirdDegreePoly = 1
            FourthDegreePoly = 2
            Linear = 3
        End Enum

        Private _x, _y As Double()
        Private sum As Double
        Private its As Integer = 0

        Public Function GetCoeffs(ByVal x As Double(), ByVal y As Double(), ByVal inest As Double(), ByVal fittype As FitType,
                                ByVal epsg As Double, ByVal epsf As Double, ByVal epsx As Double, ByVal maxits As Integer) As Tuple(Of Double(), String, Double, Integer)

            Dim lmsolve As New MathEx.LM.levenbergmarquardt
            Select Case fittype
                Case LMFit.FitType.SecondDegreePoly
                    lmsolve.DefineFuncGradDelegate(AddressOf fvsdp)
                Case LMFit.FitType.ThirdDegreePoly
                    lmsolve.DefineFuncGradDelegate(AddressOf fvstp)
                Case LMFit.FitType.FourthDegreePoly
                    lmsolve.DefineFuncGradDelegate(AddressOf fvftp)
                Case LMFit.FitType.Linear
                    lmsolve.DefineFuncGradDelegate(AddressOf fvlin)
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

            Return New Tuple(Of Double(), String, Double, Integer)(coeffs, GetInfo(info), sum, its)

        End Function

        Private Function GetInfo(code As Integer)

            Select Case code
                Case -1
                    Return "Wrong parameters were specified"
                Case 0
                    Return "Interrupted by user"
                Case 1
                    Return "Relative decrease of sum of function values squares (real and predicted on the base  of extrapolation) is less or equal EpsF"
                Case 2
                    Return "Relative change of solution Is less Or equal EpsX."
                Case 3
                    Return "Conditions (1) And (2) are fulfilled."
                Case 4
                    Return "Cosine of the angle between vector of function values and each of the Jacobian columns is less or equal EpsG by absolute value."
                Case 5
                    Return "Number of iterations exceeds MaxIts."
                Case 6
                    Return "EpsF Is too small. It is impossible to get a better result."
                Case 7
                    Return "EpsX Is too small. It Is impossible to get a better result."
                Case 8
                    Return "EpsG Is too small. Vector of functions is orthogonal to Jacobian columns with near-machine precision."
                Case Else
                    Return ""
            End Select

        End Function

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

        Public Sub fvstp(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

            If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
            If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

            'A + B * T + C * T ^ 2 + D * T ^ 3
            sum = 0.0#
            Dim i As Integer
            If iflag = 1 Then
                i = 1
                Do
                    fvec(i) = -_y(i - 1) + (x(1) + x(2) * _x(i - 1) + x(3) * _x(i - 1) ^ 2 + x(4) * _x(i - 1) ^ 3)
                    sum += (fvec(i)) ^ 2
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            ElseIf iflag = 2 Then
                i = 1
                Do
                    'A + B * T + C * T ^ 2 + D * T ^ 3
                    fjac(i, 1) = 1
                    fjac(i, 2) = _x(i - 1)
                    fjac(i, 3) = _x(i - 1) ^ 2
                    fjac(i, 4) = _x(i - 1) ^ 3
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            End If

            its += 1

        End Sub

        Public Sub fvftp(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

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

        Public Sub fvlin(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

            If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
            If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

            'A + B * T
            sum = 0.0#
            Dim i As Integer
            If iflag = 1 Then
                i = 1
                Do
                    fvec(i) = -_y(i - 1) + (x(1) + x(2) * _x(i - 1))
                    sum += (fvec(i)) ^ 2
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            ElseIf iflag = 2 Then
                i = 1
                Do
                    'A + B * T
                    fjac(i, 1) = 1
                    fjac(i, 2) = _x(i - 1)
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            End If

            its += 1

        End Sub

    End Class

End Namespace
